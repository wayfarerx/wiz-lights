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

/** A signal strength value for lights in the range -95 dBm - -20 dBm. */
opaque type Signal <: Int = Int

/**
 * Factory for light signal strength values.
 */
object Signal:

  /** The weakest possible signal. */
  val Weakest: Signal = -95

  /** The strongest possible signal. */
  val Strongest: Signal = -20

  /** The midpoint between the weakest and strongest signal values. */
  val Medium: Signal = (Strongest - Weakest) / 2 + Weakest

  /**
   * Creates a new light signal strength definition.
   *
   * @param value The signal strength value of the light in the range -90 - -30.
   * @return The new light signal strength definition if the value is valid.
   */
  def make(value: Int): Task[Signal] =
    if value >= Weakest && value <= Strongest then ZIO.succeed(value) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid signal value: $value. Must be between $Weakest and $Strongest."
      ))
