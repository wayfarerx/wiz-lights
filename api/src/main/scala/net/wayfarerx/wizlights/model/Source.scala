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

/** An update source for a light. */
opaque type Source <: String = String

/**
 * Factory for update sources.
 */
object Source:

  /** The circe encoder for update sources. */
  given Encoder[Source] = Encoder.encodeString

  /** The circe decoder for update sources. */
  given Decoder[Source] = Decoder.decodeString.emap(from)

  /** The heartbeat update source. */
  val Heartbeat: Source = "hb"

  /** The UDP command update source. */
  val Udp: Source = "udp"

  /** The mobile application update source. */
  val App: Source = "wfa"

  /** The remote control update source. */
  val Remote: Source = "wzm"

  /** The motion sensor update source. */
  val Sensor: Source = "pir"

  /**
   * Attempts to create an update source from the specified string.
   *
   * @param string The string to validate as an update source.
   * @return The validated update source or a failure.
   */
  def from(string: String): Either[String, Source] = string.trim match
    case invalid if invalid.isEmpty => Left("Empty or blank source.")
    case valid => Right(valid)

  /**
   * Creates an update source from the specified string.
   *
   * @param string The string to validate as an update source.
   * @return The validated update source.
   */
  def make(string: String): IO[IllegalArgumentException, Source] =
    ZIO.fromEither(from(string)).mapError(IllegalArgumentException(_))
