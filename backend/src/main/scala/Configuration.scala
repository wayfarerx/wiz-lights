package net.wayfarerx.wizlights
package backend

import java.io.{File, InputStream}
import java.nio.file.{Files, Paths}
import java.util.Properties

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import cats.data.{NonEmptyList, NonEmptySet}

import zio.{Task, UIO, ZIO}

import model.{Address, Location}


/**
 * The configuration properties for the backend.
 *
 * @param locations     The locations the backend can interact with.
 * @param networkPort   The network port to communicate on.
 * @param retryBackoffs The durations to wait between retrying outgoing messages.
 */
case class Configuration private(
  locations: NonEmptySet[Location],
  networkPort: Int,
  retryBackoffs: List[FiniteDuration]
)

/**
 * Factory for backend configurations.
 */
object Configuration:

  /** The default network port to communicate on. */
  val DefaultNetworkPort: Int = 38899

  /** The default durations to wait between retrying outgoing messages. */
  val DefaultRetryBackoffs: List[FiniteDuration] = 100.milliseconds :: 150.milliseconds :: 225.milliseconds :: Nil

  /** The default path of the file that defines the locations the backend can interact with. */
  val DefaultLocationsAt: NonEmptyList[String] = NonEmptyList.of("~", ".wiz-lights-locations")

  /**
   * Creates a new backend configuration.
   *
   * @param locations     The locations the backend can interact with.
   * @param networkPort   The network port to communicate on.
   * @param retryBackoffs The durations to wait between retrying outgoing messages.
   * @return A new backend configuration.
   */
  def make(
    locations: NonEmptySet[Location],
    networkPort: Int = DefaultNetworkPort,
    retryBackoffs: Iterable[FiniteDuration] = DefaultRetryBackoffs
  ): Task[Configuration] =
    validateLocations(locations) validate {
      if networkPort >= 0 && networkPort <= 65535 then ZIO.succeed(()) else
        ZIO.fail(IllegalArgumentException(s"Invalid network port: $networkPort."))
    } map (_ => Configuration(locations, networkPort, retryBackoffs.toList))

  /**
   * Validates that a set of locations contains only unique names and MAC addresses.
   *
   * @param locations The location set to validate.
   */
  private def validateLocations(locations: NonEmptySet[Location]): Task[Unit] =
    val locationList = locations.toNonEmptyList.toList
    val nameList = locationList.map(_.name)
    val macAddressList = locationList.map(_.macAddress)
    val nameValidation =
      if nameList.distinct.sizeCompare(nameList) == 0 then ZIO.succeed(()) else
        ZIO.fail(IllegalArgumentException("Duplicate location names are not allowed."))
    val macAddressValidation =
      if macAddressList.distinct.sizeCompare(macAddressList) == 0 then ZIO.succeed(()) else
        ZIO.fail(IllegalArgumentException("Duplicate location MAC addresses are not allowed."))
    nameValidation validate macAddressValidation map (_ => ())

  /**
   * Loads a new backend configuration.
   *
   * @param locationsAt   The path of the locations file to load.
   * @param networkPort   The network port to communicate on.
   * @param retryBackoffs The durations to wait between retrying outgoing messages.
   * @return A new backend configuration.
   */
  def load(
    locationsAt: NonEmptyList[String] = DefaultLocationsAt,
    networkPort: Int = DefaultNetworkPort,
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
      result <- make(actualLocations, networkPort, retryBackoffs)
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
