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

/** A group identifier within a room. */
opaque type Group <: Int = Int

/**
 * Factory for group identifiers.
 */
object Group:

  /** The ordering of group IDs. */
  given Ordering[Group] = Ordering.Int

  /** The cats order of group IDs. */
  given Order[Group] = Order.fromOrdering

  /**
   * Creates a new group identifier.
   *
   * @param value The group ID value.
   * @return The new group identifier if the value is valid.
   */
  def make(value: Int): Task[Group] =
    if value > 0 then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(s"Invalid group ID: $value. Must be positive."))
