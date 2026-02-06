package net.wayfarerx.wizlights
package model

/**
 * Base type for events emitted when a light's state changes.
 */
sealed trait Event

/**
 * Definitions of the light events.
 */
object Event:

  /**
   * Signals that a light was physically turned on.
   *
   * @param light The information about the light that was turned on.
   */
  case class TurnedOn(light: Light) extends Event

  /**
   * Signals that a light's state was updated somehow.
   *
   * @param light The information about the light that was updated.
   */
  case class Updated(light: Light) extends Event

  /**
   * Signals that a light was physically turned off.
   *
   * @param location The location of the light that was turned off.
   */
  case class TurnedOff(location: Location) extends Event
