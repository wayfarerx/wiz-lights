/*
 * Copyright (c) 2026 wayfarerx.net.
 *
 * This file is licensed to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

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