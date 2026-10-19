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

/** A light firmware version. */
opaque type Firmware <: String = String

/**
 * Factory for firmware versions.
 */
object Firmware:

  /** The ordering of firmware versions. */
  given Ordering[Firmware] = Ordering.String

  /** The cats order of firmware versions. */
  given Order[Firmware] = Order.fromOrdering

  /**
   * Creates a firmware version from the specified string.
   *
   * @param string The string to validate as a firmware version.
   * @return The validated firmware version.
   */
  def make(string: String): Task[Firmware] = string.trim match
    case invalid if invalid.isEmpty => ZIO.fail(IllegalArgumentException("Empty or blank firmware version."))
    case valid => ZIO.succeed(valid)
