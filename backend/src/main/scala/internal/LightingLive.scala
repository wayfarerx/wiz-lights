package net.wayfarerx.wizlights
package backend
package internal

import java.net.InetAddress

import scala.collection.immutable.SortedSet

import cats.data.{NonEmptyList, NonEmptySet}
import net.wayfarerx.wizlights.backend.network.{Incoming, Outgoing, Socket, SocketLive}
import net.wayfarerx.wizlights.backend.protocol.{GetPilotRequest, GetPilotResponse}
import net.wayfarerx.wizlights.model.*
import net.wayfarerx.wizlights.service.Lighting
import zio.concurrent.ConcurrentMap
import zio.stream.{UStream, ZStream}
import zio.{Clock, Duration, Hub, RLayer, Schedule, Scope, UIO, URIO, ZIO, ZLayer}

/**
 * A live implementation of the lighting service.
 *
 * @param socket  The network socket to use.
 * @param devices The devices this service is concerned with.
 * @param routing The routing table to use.
 * @param events  The event publishing hub.
 */
final class LightingLive private(
  socket: Socket,
  devices: NonEmptyList[Device],
  routing: ConcurrentMap[InetAddress, Device],
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

  /** A function that publishes an event. */
  private val publish: Event => UIO[Unit] = {
    case Event.InetAddressChanged(location, oldAddress, newAddress) =>
      newAddress.fold(ZIO.unit) { inetAddress =>
        byLocation.get(location).fold(ZIO.unit)(routing.put(inetAddress, _))
      } *> oldAddress.fold(ZIO.unit)(routing.remove) *> ZIO.unit
    case Event.StatusChanged(location, status) =>
      events.publish(Light(location, status)) *> ZIO.unit
  }

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
   * Broadcasts a "getPilot" request to the entire network.
   */
  private def discover: UIO[Unit] =
    socket.publish(Outgoing.Broadcast(GetPilotRequest))

  /**
   * Called when a message is received from the network.
   *
   * @param message The message that was received.
   */
  private def received(message: Incoming): UIO[Unit] = for
    routed <- routing.get(message.address)
    device <- routed.fold {
      message.response match
        case response: GetPilotResponse =>
          Address.make(response.mac).map(byMacAddress.get).catchAllCause { cause =>
            ZIO.logWarningCause(s"Invalid MAC address in getPilot response: ${response.mac}.", cause).map(_ => None)
          }
        case _ => ZIO.none
    }(ZIO.some)
    _ <- device.fold(ZIO.logInfo(s"Unable to route incoming message: $message.")) {
      _.received(message, publish).catchAllCause {
        ZIO.logWarningCause(s"Failed to deliver invalid incoming message: $message.", _)
      }
    }
  yield ()

  /**
   * Visitor for the "lookup" operations.
   */
  private object Lookup extends Lighting.Key.Visitor[Device]:

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
  val layer: RLayer[Clock & Configuration, Lighting] =
    SocketLive.layer >>> ZLayer.scoped {
      for
        scope <- ZIO.service[Scope]
        clock <- ZIO.service[Clock]
        config <- ZIO.service[Configuration]
        socket <- ZIO.service[Socket]
        devices <- ZIO.foldLeft(config.locations.toSortedSet)(List.empty[Device]) { (s, l) =>
          Device.make(l, clock).map(_ :: s)
        }
        routing <- ConcurrentMap.make[InetAddress, Device]()
        events <- Hub.unbounded[Light]
        service = LightingLive(socket, NonEmptyList.fromListUnsafe(devices), routing, events)
        subscription <- socket.subscribe
        _ <- subscription.foreach(service.received).forkIn(scope)
        _ <- service.discover.repeat(Schedule.fixed(Duration.fromScala(config.discoveryInterval))).forkIn(scope)
      yield service
    }
