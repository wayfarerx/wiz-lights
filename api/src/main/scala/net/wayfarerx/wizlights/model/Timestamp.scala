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

/** A timestamp representing milliseconds since the Unix epoch. */
opaque type Timestamp <: Long = Long

/**
 * Factory for timestamps.
 */
object Timestamp:

  /** The circe encoder for timestamps. */
  given Encoder[Timestamp] = Encoder.encodeLong

  /** The circe decoder for timestamps. */
  given Decoder[Timestamp] = Decoder.decodeLong.emap(from)

  /**
   * Attempts to create a timestamp from milliseconds since the epoch.
   *
   * @param value The timestamp in milliseconds since the epoch.
   * @return The new timestamp if valid or a failure.
   */
  def from(value: Long): Either[String, Timestamp] =
    if value >= 0L then Right(value)
    else Left(s"Invalid timestamp: $value. Cannot be negative.")

  /**
   * Creates a timestamp from milliseconds since the epoch.
   *
   * @param value The timestamp in milliseconds since the epoch.
   * @return The new timestamp if the value is valid.
   */
  def make(value: Long): IO[IllegalArgumentException, Timestamp] =
    ZIO.fromEither(from(value)).mapError(IllegalArgumentException(_))
