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

/**
 * The signal strength of a light.
 */
enum Signal:

  /** Good signal strength. */
  case Good

  /** Fair signal strength. */
  case Fair

  /** Poor signal strength. */
  case Poor

/**
 * Factory for light signal strength values.
 */
object Signal:

  /** The circe encoder for signal strength values. */
  given Encoder[Signal] = Encoder.encodeString.contramap(_.toString)

  /** The circe decoder for signal strength values. */
  given Decoder[Signal] = Decoder.instance { cursor =>
    if cursor.value.isNumber then Decoder.decodeInt.emap(from)(cursor)
    else Decoder.decodeString.emap(from)(cursor)
  }

  /** The weakest supported signal in dBm. */
  val Weakest: Int = -95

  /** The strongest supported signal in dBm. */
  val Strongest: Int = -20

  /**
   * Attempts to determine the light signal strength from an RSSI dBm value.
   *
   * @param value The signal strength value of the light in the range -95 dBm to -20 dBm.
   * @return The signal strength enumeration value if valid or a failure.
   */
  def from(value: Int): Either[String, Signal] =
    if value >= -60 then
      Right(Signal.Good)
    else if value >= -75 then
      Right(Signal.Fair)
    else
      Right(Signal.Poor)

  /**
   * Attempts to return the signal strength with the specified name.
   *
   * @param name The name of the signal strength to return.
   * @return The signal strength with the specified name or a failure.
   */
  def from(name: String): Either[String, Signal] =
    name.trim.toLowerCase match
      case "good" => Right(Signal.Good)
      case "fair" => Right(Signal.Fair)
      case "poor" => Right(Signal.Poor)
      case _ => Left(s"Invalid signal name: $name.")

  /**
   * Creates a light signal strength definition from an RSSI dBm value.
   *
   * @param value The signal strength value of the light in the range -95 to -20.
   * @return The signal strength enumeration value if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Signal] =
    ZIO.succeed(from(value).toOption.get)

  /**
   * Creates a light signal strength definition from a name.
   *
   * @param name The name of the signal strength.
   * @return The signal strength enumeration value if the name is valid.
   */
  def make(name: String): IO[IllegalArgumentException, Signal] =
    ZIO.fromEither(from(name)).mapError(IllegalArgumentException(_))
