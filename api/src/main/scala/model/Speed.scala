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

import zio.{Task, ZIO}

/** A speed value for dynamic scenes in the range 10 - 100. */
opaque type Speed <: Int = Int

/**
 * Factory for dynamic scene speed values.
 */
object Speed:

  /** The slowest possible speed. */
  val Slowest: Speed = 10

  /** The fastest possible speed. */
  val Fastest: Speed = 200

  /** The midpoint between the slowest and fastest speed values. */
  val Medium: Speed = (Fastest - Slowest) / 2 + Slowest

  /**
   * Creates a new dynamic scene speed definition.
   *
   * @param value The speed value of the dynamic scene in the range 10 - 100.
   * @return The new dynamic scene speed definition if the value is valid.
   */
  def make(value: Int): Task[Speed] =
    if value >= Slowest && value <= Fastest then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid speed value: $value. Must be between $Slowest and $Fastest."
      ))
