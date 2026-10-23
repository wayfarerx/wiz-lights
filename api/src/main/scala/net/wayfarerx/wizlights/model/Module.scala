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

/** A light module name. */
opaque type Module <: String = String

/**
 * Factory for module names.
 */
object Module:

  /** The circe encoder for module names. */
  given Encoder[Module] = Encoder.encodeString

  /** The circe decoder for module names. */
  given Decoder[Module] = Decoder.decodeString.emap(from)

  /**
   * Attempts to create a module name from the specified string.
   *
   * @param string The string to validate as a module name.
   * @return The validated module name or a failure.
   */
  def from(string: String): Either[String, Module] = string.trim match
    case invalid if invalid.isEmpty => Left("Empty or blank module name.")
    case valid => Right(valid)

  /**
   * Creates a module name from the specified string.
   *
   * @param string The string to validate as a module name.
   * @return The validated module name.
   */
  def make(string: String): IO[IllegalArgumentException, Module] =
    ZIO.fromEither(from(string)).mapError(IllegalArgumentException(_))
