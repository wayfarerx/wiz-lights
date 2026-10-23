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

/** A group identifier within a room. */
opaque type Group <: Int = Int

/**
 * Factory for group identifiers.
 */
object Group:

  /** The circe encoder for group IDs. */
  given Encoder[Group] = Encoder.encodeInt

  /** The circe decoder for group IDs. */
  given Decoder[Group] = Decoder.decodeInt.emap(from)

  /**
   * Attempts to create a new group identifier.
   *
   * @param value The group ID value.
   * @return The new group identifier if valid or a failure.
   */
  def from(value: Int): Either[String, Group] =
    if value > 0 then Right(value) else Left(s"Invalid group ID: $value. Must be positive.")

  /**
   * Creates a new group identifier.
   *
   * @param value The group ID value.
   * @return The new group identifier if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Group] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
