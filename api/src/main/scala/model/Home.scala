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

/** A unique home identifier. */
opaque type Home <: Int = Int

/**
 * Factory for home identifiers.
 */
object Home:

  /** The circe encoder for home IDs. */
  given Encoder[Home] = Encoder.encodeInt

  /** The circe decoder for home IDs. */
  given Decoder[Home] = Decoder.decodeInt.emap(from)

  /**
   * Attempts to create a new home identifier.
   *
   * @param value The home ID value.
   * @return The new home identifier if valid or a failure.
   */
  def from(value: Int): Either[String, Home] =
    if value > 0 then Right(value) else Left(s"Invalid home ID: $value. Must be positive.")

  /**
   * Creates a new home identifier.
   *
   * @param value The home ID value.
   * @return The new home identifier if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Home] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
