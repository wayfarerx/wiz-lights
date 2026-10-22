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

/** A light device type identifier. */
opaque type Device <: Int = Int

/**
 * Factory for device type identifiers.
 */
object Device:

  /** The circe encoder for device type identifiers. */
  given Encoder[Device] = Encoder.encodeInt

  /** The circe decoder for device type identifiers. */
  given Decoder[Device] = Decoder.decodeInt.emap(from)

  /**
   * Attempts to create a new device type identifier.
   *
   * @param value The device type ID value.
   * @return The new device type identifier if valid or a failure.
   */
  def from(value: Int): Either[String, Device] =
    if value >= 0 then Right(value)
    else Left(s"Invalid device type ID: $value. Cannot be negative.")

  /**
   * Creates a new device type identifier.
   *
   * @param value The device type ID value.
   * @return The new device type identifier if the value is valid.
   */
  def make(value: Int): IO[IllegalArgumentException, Device] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
