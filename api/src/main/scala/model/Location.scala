package net.wayfarerx.wizlights
package model

import cats.kernel.Order

/**
 * A location that a light is expected to be found at.
 *
 * @param name       The name of this location.
 * @param macAddress The MAC address of the light at this location.
 */
case class Location(name: String, macAddress: String)

/**
 * Definitions associated with locations.
 */
object Location:

  /** The order of locations. */
  given Ordering[Location] = Ordering.by(_.macAddress)

  /** The cats order of locations. */
  given Order[Location] = Order.fromOrdering
