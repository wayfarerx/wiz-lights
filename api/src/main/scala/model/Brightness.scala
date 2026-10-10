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

/**
 * A brightness value for lights.
 *
 * @param value The brightness value of the light in the range 10 - 100.
 */
case class Brightness private(value: Int)

/**
 * Factory for light brightness values.
 */
object Brightness:

  /** The dimmest possible brightness. */
  val Dimmest = Brightness(10)

  /** The brightest possible brightness. */
  val Brightest = Brightness(100)

  /** The midpoint between the dimmest and brightest brightness values. */
  val Medium = Brightness((Brightest.value - Dimmest.value) / 2 + Dimmest.value)

  /**
   * Creates a new light brightness definition.
   *
   * @param value The brightness value of the light in the range 10 - 100.
   * @return The new light brightness definition if the value is valid.
   */
  def make(value: Int): Task[Brightness] =
    if value >= Dimmest.value && value <= Brightest.value then ZIO.succeed(Brightness(value)) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid brightness value: $value. Must be between ${Dimmest.value} and ${Brightest.value}."
      ))
