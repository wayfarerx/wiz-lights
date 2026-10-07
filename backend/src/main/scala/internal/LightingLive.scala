package net.wayfarerx.wizlights
package backend
package internal

import scala.collection.immutable.SortedSet

import cats.data.{NonEmptyList, NonEmptySet}

import zio.concurrent.ConcurrentMap
import zio.stream.{UStream, ZStream}
import zio.{Hub, RLayer, Scope, UIO, URIO, ZIO, ZLayer}

import backend.network.{Socket, SocketLive}
import model.*
import service.Lighting

/**
 * A live implementation of the lighting service.
 *
 * @param devices The collection of devices this service is concerned about.
 * @param events  The event publishing hub.
 */
case class LightingLive private(
  devices: NonEmptyList[Device],
  events: Hub[Light]
) extends Lighting:

  /** The list of all devices. */
  private val deviceList = devices.toList

  /** The index of devices by name. */
  private val byName = deviceList.map(d => d.name -> d).toMap

  /** The index of devices by MAV address. */
  private val byMacAddress = deviceList.map(d => d.macAddress -> d).toMap

  /** The index of devices by location. */
  private val byLocation = deviceList.map(d => d.location -> d).toMap

  /* Return the lights managed by this service. */
  override def lights: UIO[NonEmptySet[Light]] = for
    lights <- ZIO.foldLeft(deviceList)(SortedSet.empty[Light])((lights, device) => device.toLight.map(lights + _))
  yield NonEmptySet.fromSetUnsafe(lights)

  /* Return the light with the specified key. */
  override def get[Key: Lighting.Key](key: Key): UIO[Option[Light]] = for
    device <- Lookup(key)
    light <- device.fold(ZIO.none)(_.toLight.map(Some.apply))
  yield light

  /* Return the lights with the specified keys. */
  override def getAll[Key: Lighting.Key](keys: Iterable[Key]): UIO[Set[Light]] = for
    devices <- Lookup(keys)
    lights <- ZIO.foldLeft(devices)(Set.empty[Light])((s, l) => l.toLight.map(s + _))
  yield lights

  /* Update the status of the light with the specified key. */
  override def update[Key: Lighting.Key](key: Key, status: Status): UIO[Boolean] = for
    device <- Lookup(key)
  yield ??? // FIXME

  /* Update the status of the lights with the specified keys. */
  override def updateAll[Key: Lighting.Key](keys: Iterable[Key], status: Status): UIO[Int] = for
    devices <- Lookup(keys)
  yield ??? // FIXME

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[Light]] =
    ZStream.fromHubScoped(events)

  /**
   * Visitor for the "lookup" operations.
   */
  private object Lookup extends Lighting.Key.Visitor[Device]:

    /**
     * Looks up a device with the specified key.
     *
     * @tparam Key The type of key to look up.
     * @param key The key to look up.
     * @return The device with the specified key.
     */
    def apply[Key: Lighting.Key](key: Key): UIO[Option[Device]] =
      summon[Lighting.Key[Key]].apply(key, this)

    /**
     * Looks up the devices with the specified keys.
     *
     * @tparam Key The type of key to look up.
     * @param keys The keys to look up.
     * @return The devices with the specified keys.
     */
    def apply[Key: Lighting.Key](keys: Iterable[Key]): UIO[List[Device]] =
      summon[Lighting.Key[Key]].apply(keys, this)

    /* Called when the key type is a name. */
    override def onName(name: String): UIO[Option[Device]] =
      ZIO.succeed(byName.get(name))

    /* Called when the key type is a collection of names. */
    override def onNames(names: Iterable[String]): UIO[List[Device]] =
      ZIO.succeed(names.flatMap(byName.get).toList)

    /* Called when the key type is a MAC address. */
    override def onAddress(macAddress: Address): UIO[Option[Device]] =
      ZIO.succeed(byMacAddress.get(macAddress))

    /* Called when the key type is a collection of MAC addresses. */
    override def onAddresses(macAddresses: Iterable[Address]): UIO[List[Device]] =
      ZIO.succeed(macAddresses.flatMap(byMacAddress.get).toList)

    /* Called when the key type is a location. */
    override def onLocation(location: Location): UIO[Option[Device]] =
      ZIO.succeed(byLocation.get(location))

    /* Called when the key type is a collection of locations. */
    override def onLocations(locations: Iterable[Location]): UIO[List[Device]] =
      ZIO.succeed(locations.flatMap(byLocation.get).toList)

/**
 * Factory for live lighting services.
 */
object LightingLive:

  /** A layer that contains a live lighting service. */
  val layer: RLayer[Configuration, Lighting] =
    SocketLive.layer >>> ZLayer.scoped {
      for
        config <- ZIO.service[Configuration]
        socket <- ZIO.service[Socket]
        scope <- ZIO.service[Scope]
        lights <- ConcurrentMap.make[Location, Light]()
        events <- Hub.unbounded[Light]
        //        service = LightingLive(
        //          socket,
        //          
        //        ) FIXME
        subscription <- socket.subscribe
      // _ <- subscription.foreach(service.received).forkIn(scope)
      yield ??? // service
    }
