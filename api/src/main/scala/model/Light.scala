package net.wayfarerx.wizlights
package model

import cats.kernel.Order

/**
 * Information about a powered light.
 *
 * @param location The location of this light.
 * @param status   The state of this light if it is powered on.
 */
case class Light(location: Location, status: Option[Status]):

  /** Returns the name of this light. */
  def name: String = location.name

/**
 * Definitions associated with lights.
 */
object Light:

  /** The ordering of lights. */
  given Ordering[Light] = Ordering.by[Light, Location](_.location)

  /** The cats order of lights. */
  given Order[Light] = Order.fromOrdering

