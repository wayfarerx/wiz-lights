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

/** A unique home identifier. */
opaque type Home <: Int = Int

/**
 * Factory for home identifiers.
 */
object Home:

  /** The ordering of home IDs. */
  given Ordering[Home] = Ordering.Int

  /** The cats order of home IDs. */
  given Order[Home] = Order.fromOrdering

  /**
   * Creates a new home identifier.
   *
   * @param value The home ID value.
   * @return The new home identifier if the value is valid.
   */
  def make(value: Int): Task[Home] =
    if value > 0 then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(s"Invalid home ID: $value. Must be positive."))
