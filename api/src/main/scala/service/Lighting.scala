package net.wayfarerx.wizlights
package service

import model.*

import zio.stream.UStream
import zio.{Scope, UIO, URIO}

/**
 * A service that interacts with available lights.
 */
trait Lighting:

  /**
   * Looks up a powered light.
   *
   * @param location The location of the powered light to look up.
   * @return The specified powered light if it is available.
   */
  def get(location: Location): UIO[Option[Light]]

  /**
   * Looks up a set of powered lights.
   *
   * @param locations The locations of the powered lights to look up.
   * @return Any of the specified powered lights that are available.
   */
  def get(locations: Locations): UIO[Map[Location, Light]]

  /**
   * Sets the state of the light at the specified location.
   *
   * @param location The location of the light to set the state of.
   * @param state    The state to set on the light at the specified location.
   * @return A promise to set the state of the specified light.
   */
  def set(location: Location, state: Light.State): UIO[Unit]

  /**
   * Sets the state of the lights at the specified locations.
   *
   * @param locations The locations of the lights to set the state of.
   * @param state     The state to set on the lights at the specified locations.
   * @return A promise to set the state of the specified lights.
   */
  def set(locations: Locations, state: Light.State): UIO[Unit]

  /**
   * Subscribes to events from this service.
   *
   * @return A stream of events published by this service bound to the required scope.
   */
  def subscribe: URIO[Scope, UStream[Event]]
