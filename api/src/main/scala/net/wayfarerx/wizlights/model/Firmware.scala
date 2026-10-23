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

/** A light firmware version. */
opaque type Firmware <: String = String

/**
 * Factory for firmware versions.
 */
object Firmware:

  /** The circe encoder for firmware versions. */
  given Encoder[Firmware] = Encoder.encodeString

  /** The circe decoder for firmware versions. */
  given Decoder[Firmware] = Decoder.decodeString.emap(from)

  /**
   * Attempts to create a firmware version from the specified string.
   *
   * @param string The string to validate as a firmware version.
   * @return The validated firmware version or a failure.
   */
  def from(string: String): Either[String, Firmware] = string.trim match
    case invalid if invalid.isEmpty => Left("Empty or blank firmware version.")
    case valid => Right(valid)

  /**
   * Creates a firmware version from the specified string.
   *
   * @param string The string to validate as a firmware version.
   * @return The validated firmware version.
   */
  def make(string: String): IO[IllegalArgumentException, Firmware] =
    ZIO.fromEither(from(string)).mapError(IllegalArgumentException(_))
