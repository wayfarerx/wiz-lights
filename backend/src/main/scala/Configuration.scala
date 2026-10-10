package net.wayfarerx.wizlights
package backend

import java.io.{File, InputStream}
import java.nio.file.{Files, Paths}
import java.util.Properties

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.data.{NonEmptyList, NonEmptySet}
import net.wayfarerx.wizlights.model.{Address, Location}
import zio.{Task, UIO, ZIO}


/**
 * The configuration properties for the backend.
 *
 * @param locations         The locations the backend can interact with.
 * @param networkPort       The network port to communicate on.
 * @param discoveryInterval The frequency of broadcast discovery attempts.
 * @param retryBackoffs     The durations to wait between retrying outgoing messages.
 */
case class Configuration private(
  locations: NonEmptySet[Location],
  networkPort: Int,
  discoveryInterval: FiniteDuration,
  retryBackoffs: List[FiniteDuration]
)

/**
 * Factory for backend configurations.
 */
object Configuration:

  /** The default network port to communicate on. */
  val DefaultNetworkPort: Int = 38899

  /** The default frequency of broadcast discovery attempts. */
  val DefaultDiscoveryInterval: FiniteDuration = 5.seconds

  /** The default durations to wait between retrying outgoing messages. */
  val DefaultRetryBackoffs: List[FiniteDuration] = 100.milliseconds :: 150.milliseconds :: 225.milliseconds :: Nil

  /** The default path of the file that defines the locations the backend can interact with. */
  val DefaultLocationsAt: NonEmptyList[String] = NonEmptyList.of("~", ".wiz-lights-locations")

  /**
   * Creates a new backend configuration.
   *
   * @param locations         The locations the backend can interact with.
   * @param networkPort       The network port to communicate on.
   * @param discoveryInterval The frequency of broadcast discovery attempts.
   * @param retryBackoffs     The durations to wait between retrying outgoing messages.
   * @return A new backend configuration.
   */
  def make(
    locations: NonEmptySet[Location],
    networkPort: Int = DefaultNetworkPort,
    discoveryInterval: FiniteDuration = DefaultDiscoveryInterval,
    retryBackoffs: Iterable[FiniteDuration] = DefaultRetryBackoffs
  ): Task[Configuration] =
    validLocations(locations) validate
      validNetworkPort(networkPort) validate
      validDiscoveryInterval(discoveryInterval) validate
      validRetryBackoffs(retryBackoffs) map
      (_ => Configuration(locations, networkPort, discoveryInterval, retryBackoffs.toList))

  /**
   * Validates that a set of locations contains only unique names and MAC addresses.
   *
   * @param locations The location set to validate.
   */
  private def validLocations(locations: NonEmptySet[Location]): Task[Unit] =
    val locationList = locations.toNonEmptyList.toList
    validLocationNames(locationList.map(_.name)) validate
      validLocationAddresses(locationList.map(_.macAddress)) map
      (_ => ())

  /**
   * Validates that a list of location names is unique.
   *
   * @param names Thw list of location names that must be unique.
   */
  private def validLocationNames(names: List[String]): Task[Unit] =
    if names.distinct.sizeCompare(names) == 0 then ZIO.unit else
      ZIO.fail(IllegalArgumentException("Duplicate location names are not allowed."))

  /**
   * Validates that a list of location MAC addressed is unique.
   *
   * @param macAddresses Thw list of location MAC addressed that must be unique.
   */
  private def validLocationAddresses(macAddresses: List[Address]): Task[Unit] =
    if macAddresses.distinct.sizeCompare(macAddresses) == 0 then ZIO.unit else
      ZIO.fail(IllegalArgumentException("Duplicate location MAC addresses are not allowed."))

  /**
   * Validates that the network port is a valid IP port.
   *
   * @param networkPort The network port to validate.
   */
  private def validNetworkPort(networkPort: Int): Task[Unit] =
    if networkPort >= 0 && networkPort <= 65535 then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid network port: $networkPort."))

  /**
   * Validates that the discovery interval is a positive duration.
   *
   * @param discoveryInterval The discovery interval to validate.
   */
  private def validDiscoveryInterval(discoveryInterval: FiniteDuration): Task[Unit] =
    if discoveryInterval > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid discovery interval: $discoveryInterval."))

  /**
   * Validates that the retry backoffs are all positive durations.
   *
   * @param retryBackoffs The retry backoffs to validate.
   */
  private def validRetryBackoffs(retryBackoffs: Iterable[FiniteDuration]): Task[Unit] =
    retryBackoffs.foldLeft(ZIO attempt ())((u, r) => u validate validRetryBackoff(r) map (_ => ()))

  /**
   * Validates that a retry backoff is a positive duration.
   *
   * @param retryBackoff The retry backoff to validate.
   */
  private def validRetryBackoff(retryBackoff: FiniteDuration): Task[Unit] =
    if retryBackoff > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid retry backoff: $retryBackoff."))

  /**
   * Loads a new backend configuration.
   *
   * @param locationsAt       The path of the locations file to load.
   * @param networkPort       The network port to communicate on.
   * @param discoveryInterval The frequency of broadcast discovery attempts.
   * @param retryBackoffs     The durations to wait between retrying outgoing messages.
   * @return A new backend configuration.
   */
  def load(
    locationsAt: NonEmptyList[String] = DefaultLocationsAt,
    networkPort: Int = DefaultNetworkPort,
    discoveryInterval: FiniteDuration = DefaultDiscoveryInterval,
    retryBackoffs: Iterable[FiniteDuration] = DefaultRetryBackoffs
  ): Task[Configuration] =
    val properties = Properties()
    for
      _ <- ZIO.acquireReleaseWith(acquireLocations(locationsAt))(releaseLocations)(ZIO attempt properties.load(_))
      maybeLocations <- ZIO.foldLeft(properties.entrySet.asScala)(List.empty[Location]) { (locations, entry) =>
        for
          macAddress <- Address.make(entry.getKey.toString)
          location <- Location.make(entry.getValue.toString, macAddress)
        yield location :: locations
      }
      actualLocations <- NonEmptySet.fromSet(SortedSet.from(maybeLocations)).fold(
        ZIO.fail(IllegalStateException(s"No device locations found at ${locationsPath(locationsAt)}."))
      )(ZIO.succeed)
      result <- make(actualLocations, networkPort, discoveryInterval, retryBackoffs)
    yield result

  /**
   * Acquires an input stream to the specified locations file.
   *
   * @param locations The path to the locations file to acquire.
   * @return An input stream to the specified locations file.
   */
  private def acquireLocations(locations: NonEmptyList[String]): Task[InputStream] =
    ZIO.attempt(Files.newInputStream(Paths.get(locations.head, locations.tail *)))

  /**
   * Releases an input stream from a locations file,
   *
   * @param locations The locations file stream to release.
   */
  private def releaseLocations(locations: InputStream): UIO[Unit] =
    ZIO.attempt(locations.close()).catchAll(_ => ZIO.unit)

  /**
   * Formats a locations file path for diagnostic purposes.
   *
   * @param locations The path to the locations file to format.
   * @return The formatted locations file path.
   */
  private def locationsPath(locations: NonEmptyList[String]): String =
    locations.toList.mkString(File.separator).replaceAll("[\\\\/]+", File.separator)
