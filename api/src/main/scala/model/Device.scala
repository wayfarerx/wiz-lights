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

/** A light device type identifier. */
opaque type Device <: Int = Int

/**
 * Factory for device type identifiers.
 */
object Device:

  /** The ordering of device type IDs. */
  given Ordering[Device] = Ordering.Int

  /** The cats order of device type IDs. */
  given Order[Device] = Order.fromOrdering

  /**
   * Creates a new device type identifier.
   *
   * @param value The device type ID value.
   * @return The new device type identifier if the value is valid.
   */
  def make(value: Int): Task[Device] =
    if value >= 0 then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(s"Invalid device type ID: $value. Cannot be negative."))
