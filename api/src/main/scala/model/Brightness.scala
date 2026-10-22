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

import io.circe.{Decoder, Encoder}

import zio.{IO, ZIO}

/** A brightness value for lights in the range 10 - 100. */
opaque type Brightness <: Int = Int

/**
 * Factory for light brightness values.
 */
object Brightness:

  /** The circe encoder for brightness values. */
  given Encoder[Brightness] = Encoder.encodeInt

  /** The circe decoder for brightness values. */
  given Decoder[Brightness] = Decoder.decodeInt.emap(from)

  /** The dimmest possible brightness. */
  val Dimmest: Brightness = 10

  /** The brightest possible brightness. */
  val Brightest: Brightness = 100

  /** The midpoint between the dimmest and brightest brightness values. */
  val Medium: Brightness = (Brightest - Dimmest) / 2 + Dimmest

  /**
   * Attempts to create a light brightness definition.
   *
   * @param value The brightness value of the light in the range 10 - 100.
   * @return The new light brightness definition if valid or a failure.
   */
  def from(value: Int): Either[String, Brightness] =
    if value >= Dimmest && value <= Brightest then Right(value)
    else Left(s"Invalid brightness value: $value. Must be between $Dimmest and $Brightest.")

  /**
   * Creates a new light brightness definition.
   *
   * @param value The brightness value of the light in the range 10 - 100.
   * @return The new light brightness definition if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Brightness] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
