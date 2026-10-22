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

/** A temperature value for white lights in the range 2,200 - 6,500. */
opaque type Temperature <: Int = Int

/**
 * Factory for white light temperature values.
 */
object Temperature:

  /** The circe encoder for temperature values. */
  given Encoder[Temperature] = Encoder.encodeInt

  /** The circe decoder for temperature values. */
  given Decoder[Temperature] = Decoder.decodeInt.emap(from)

  /** The warmest possible temperature. */
  val Warmest: Temperature = 2200

  /** The coolest possible temperature. */
  val Coolest: Temperature = 6500

  /** The midpoint between the warmest and coolest temperatures. */
  val Medium: Temperature = (Coolest - Warmest) / 2 + Warmest

  /**
   * Attempts to create a new white light temperature definition.
   *
   * @param value The temperature value in kelvin of the light in the range 2,200 - 6,500.
   * @return The new white light temperature definition if valid or a failure.
   */
  def from(value: Int): Either[String, Temperature] =
    if value >= Warmest && value <= Coolest then Right(value) else
      Left(s"Invalid temperature value: $value. Must be between $Warmest and $Coolest.")

  /**
   * Creates a new white light temperature definition.
   *
   * @param value The temperature value in kelvin of the light in the range 2,200 - 6,500.
   * @return The new white light temperature definition if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Temperature] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
