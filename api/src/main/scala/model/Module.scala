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

/** A light module name. */
opaque type Module <: String = String

/**
 * Factory for module names.
 */
object Module:

  /** The ordering of module names. */
  given Ordering[Module] = Ordering.String

  /** The cats order of module names. */
  given Order[Module] = Order.fromOrdering

  /**
   * Creates a module name from the specified string.
   *
   * @param string The string to validate as a module name.
   * @return The validated module name.
   */
  def make(string: String): Task[Module] = string.trim match
    case invalid if invalid.isEmpty => ZIO.fail(IllegalArgumentException("Empty or blank module name."))
    case valid => ZIO.succeed(valid)
