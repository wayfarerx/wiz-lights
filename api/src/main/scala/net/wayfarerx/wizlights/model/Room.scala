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

/** A unique room identifier within a home. */
opaque type Room <: Int = Int

/**
 * Factory for room identifiers.
 */
object Room:

  /** The circe encoder for room IDs. */
  given Encoder[Room] = Encoder.encodeInt

  /** The circe decoder for room IDs. */
  given Decoder[Room] = Decoder.decodeInt.emap(from)

  /**
   * Attempts to create a new room identifier.
   *
   * @param value The room ID value.
   * @return The new room identifier if valid or a failure.
   */
  def from(value: Int): Either[String, Room] =
    if value > 0 then Right(value) else Left(s"Invalid room ID: $value. Must be positive.")

  /**
   * Creates a new room identifier.
   *
   * @param value The room ID value.
   * @return The new room identifier if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Room] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
