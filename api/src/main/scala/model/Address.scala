package net.wayfarerx.wizlights
package model

import cats.kernel.Order

import zio.{Task, ZIO}

/** A lower-case representation of a MAC address. */
opaque type Address = String

/**
 * Factory for MAC addresses.
 */
object Address:

  /** The ordering of MAC addresses. */
  given Ordering[Address] = Ordering.String

  /** The cats order of MAC addresses. */
  given Order[Address] = Order.fromOrdering

  /** A pattern that matches valid MAC address strings. */
  private val ValidMacAddress = "[0-9a-f]{12}".r

  /**
   * Creates a MAC address from the specified string.
   *
   * @param string The string to validate as a MAC address.
   * @return The validated MAC address.
   */
  def make(string: String): Task[Address] = string.toLowerCase.trim match
    case invalid if invalid.isEmpty => ZIO.fail(IllegalArgumentException("Empty or blank MAC address."))
    case ValidMacAddress(macAddress) => ZIO.succeed(macAddress)
    case malformed => ZIO.fail(IllegalArgumentException(s"Malformed MAC address: $malformed."))