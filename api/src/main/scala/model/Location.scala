package net.wayfarerx.wizlights
package model

import cats.kernel.Order
import zio.{Task, ZIO}

/**
 * A location that a light is expected to be found at.
 *
 * @param name       The name of this location.
 * @param macAddress The MAC address of the light at this location.
 */
case class Location private(name: String, macAddress: String)

/**
 * Definitions associated with locations.
 */
object Location:

  /** The order of locations. */
  given Ordering[Location] = Ordering.by(_.macAddress)

  /** The cats order of locations. */
  given Order[Location] = Order.fromOrdering

  /** A pattern that matches valid mac addresses. */
  private val MacAddressPattern = "[0-9a-f]{12}".r

  /**
   * Creates a new location.
   *
   * @param name       The name of the location.
   * @param macAddress The MAC address of the light at the location.
   * @return A new location if the name and MAC address are valid.
   */
  def make(name: String, macAddress: String): Task[Location] =
    for (n, m) <- validName(name) validate validMacAddress(macAddress) yield Location(n, m)

  /**
   * Validates a location name.
   *
   * @param name The location name to validate.
   * @return The specified name if it is valid.
   */
  private def validName(name: String) =
    val trimmed = name.trim
    if trimmed.isEmpty then ZIO.fail(IllegalArgumentException("Invalid empty or blank location name."))
    else ZIO.succeed(trimmed)

  /**
   * Validates a location MAC address.
   *
   * @param macAddress The location MAC address to validate.
   * @return The specified MAC address if it is valid.
   */
  private def validMacAddress(macAddress: String) =
    val lowerTrimmed = macAddress.toLowerCase.trim
    if lowerTrimmed.isEmpty then ZIO.fail(IllegalArgumentException("Invalid empty or blank location MAC address."))
    else if MacAddressPattern.matches(lowerTrimmed) then ZIO.succeed(lowerTrimmed)
    else ZIO.fail(IllegalArgumentException(s"Malformed location MAC address: $macAddress."))
