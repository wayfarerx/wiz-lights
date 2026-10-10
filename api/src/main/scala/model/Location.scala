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

/**
 * The location of a device.
 *
 * @param name       The name of this location.
 * @param macAddress The MAC address of the device at this location.
 */
case class Location private(name: String, macAddress: Address)

/**
 * Definitions associated with locations.
 */
object Location:

  /** The ordering of locations. */
  given Ordering[Location] = Ordering.by[Location, String](_.name).orElseBy(_.macAddress)

  /** The cats order of locations. */
  given Order[Location] = Order.fromOrdering

  /**
   * Creates a new location.
   *
   * @param name       The name of the location.
   * @param macAddress The MAC address of the device at the location.
   * @return The new location.
   */
  def make(name: String, macAddress: Address): Task[Location] = name.trim match
    case invalid if invalid.isEmpty => ZIO.fail(IllegalArgumentException("Empty or blank location name."))
    case valid => ZIO.succeed(Location(valid, macAddress))