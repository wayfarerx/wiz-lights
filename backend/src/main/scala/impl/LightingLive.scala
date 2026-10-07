package net.wayfarerx.wizlights
package backend
package impl

import cats.data.NonEmptyList
import model.*
import net.wayfarerx.wizlights.backend.impl
import network.*
import service.*
import zio.IsSubtypeOfError.impl
import zio.concurrent.ConcurrentMap
import zio.stream.{UStream, ZStream}
import zio.{Hub, RLayer, Ref, Scope, UIO, URIO, ZIO, ZLayer}

import java.net.InetAddress
import scala.concurrent.duration.FiniteDuration

/**
 * A live implementation of the lighting service.
 *
 * @param socket            The socket to communicate with.
 * @param devices           The collection of devices this service is concerned about.
 * @param sendRetries       The delays between retrying send operations.
 * @param invocationCounter The counter to use when identifying invocations.
 * @param events            The event publishing hub.
 */
case class LightingLive private(
  socket: Socket,
  devices: Map[Location, Device],
  sendRetries: NonEmptyList[FiniteDuration],
  invocationCounter: Ref[Long],
  events: Hub[Event]
) extends Lighting:

  /* Look up a powered light. */
  override def get(location: Location): UIO[Option[Light]] =
    devices.get(location).fold(ZIO.none)(_.toLight)

  /* Look up a set of powered lights. */
  override def get(locations: Locations): UIO[Map[Location, Light]] =
    ZIO.foldLeft(lookup(locations).values)(Map.empty[Location, Light]) { (result, device) =>
      device.toLight.map(_.fold(result)(light => result + (device.location -> light)))
    }

  /* Set the state of the light at the specified location. */
  override def set(location: Location, state: Light.State): UIO[Unit] =
    send(Locations(location), state)

  /* Set the state of the lights at the specified locations. */
  override def set(locations: Locations, state: Light.State): UIO[Unit] =
    send(locations, state)

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[Event]] =
    ZStream.fromHubScoped(events)

  private def send(locations: Locations, state: Light.State): UIO[Unit] = {
    val invocation = invocationCounter.incrementAndGet
    val devices = lookup(locations)

    def sendLoop(retries: NonEmptyList[FiniteDuration]): UIO[Unit] = for
      addresses <- ZIO.foreach(devices) { case (location, device) =>
        for
          currentVersion <- device.currentVersion
          ipAddress <- if versions.get(location).contains(currentVersion) then device.ipAddress else ZIO.none
        yield ipAddress
      }.map(_.flatten.toSet)
      _ <- if addresses.isEmpty then ZIO.unit else {
        socket.publish(Outgoing.Multicast(addresses, ???))
      }
    yield ()

    sendLoop(sendRetries)
  }.fork.unit

  private def received(incoming: Incoming): UIO[Unit] =
    ???

  /**
   * Looks up the devices at the specifeid locations.
   *
   * @param locations The locations of the devices to look up.
   * @return The devices at the specified locations.
   */
  private def lookup(locations: Locations): Map[Location, Device] =
    locations.foldLeft(Map.empty[Location, Device]) { (result, location) =>
      devices.get(location).fold(result)(device => result + (location -> device))
    }

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
        events <- Hub.unbounded[Event]
        service = LightingLive()
        subscription <- socket.subscribe
        _ <- subscription.foreach(service.received).forkIn(scope)
      yield service
    }
