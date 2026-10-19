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

/** A unique room identifier within a home. */
opaque type Room <: Int = Int

/**
 * Factory for room identifiers.
 */
object Room:

  /** The ordering of room IDs. */
  given Ordering[Room] = Ordering.Int

  /** The cats order of room IDs. */
  given Order[Room] = Order.fromOrdering

  /**
   * Creates a new room identifier.
   *
   * @param value The room ID value.
   * @return The new room identifier if the value is valid.
   */
  def make(value: Int): Task[Room] =
    if value > 0 then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(s"Invalid room ID: $value. Must be positive."))
