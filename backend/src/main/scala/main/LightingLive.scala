package net.wayfarerx.wizlights
package backend
package main

import zio.concurrent.ConcurrentMap
import zio.stream.{UStream, ZStream}
import zio.{Hub, Scope, UIO, URIO, RLayer, ZIO, ZLayer}

import model.*
import service.*

/**
 * A live implementation of the lighting service.
 *
 * @param lights The collection of powered-on lights by location.
 * @param events The event publishing hub.
 */
case class LightingLive private(lights: ConcurrentMap[Location, Light], events: Hub[Event]) extends Lighting:

  /* Look up a powered light. */
  override def get(location: Location): UIO[Option[Light]] =
    lights.get(location)

  /* Look up a set of powered lights. */
  override def get(locations: Locations): UIO[Map[Location, Light]] =
    ZIO.foldLeft(locations.toSortedSet)(Map.empty[Location, Light]) { case (result, location) =>
      lights.get(location).map(_.fold(result)(light => result + (light.location -> light)))
    }

  /* Set the state of the light at the specified location. */
  override def set(location: Location, state: Light.State): UIO[Unit] =
    set(Locations(location), state)

  /* Set the state of the lights at the specified locations. */
  override def set(locations: Locations, state: Light.State): UIO[Unit] =
    ???

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[Event]] =
    ZStream.fromHubScoped(events)

/**
 * Factory for live lighting services.
 */
object LightingLive:

  /** A layer that contains a live lighting service. */
  val layer: RLayer[Configuration, Lighting] = ZLayer.scoped {
    for
      config <- ZIO.service[Configuration]
      lights <- ConcurrentMap.make[Location, Light]()
      events <- Hub.unbounded[Event]
    yield LightingLive(lights, events)
  }
