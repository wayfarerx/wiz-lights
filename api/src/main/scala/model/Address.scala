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

/** A lower-case representation of a MAC address. */
opaque type Address <: String = String

/**
 * Factory for MAC addresses.
 */
object Address:

  /** The circe encoder for MAC addresses. */
  given Encoder[Address] = Encoder.encodeString

  /** The circe decoder for MAC addresses. */
  given Decoder[Address] = Decoder.decodeString.emap(from)

  /** A pattern that matches valid MAC address strings. */
  private val ValidAddress = "([0-9a-f]{12})".r

  /**
   * Attempts to create a MAC address from the specified string.
   *
   * @param string The string to validate as a MAC address.
   * @return The validated MAC address or a failure.
   */
  def from(string: String): Either[String, Address] = string.toLowerCase.trim match
    case invalid if invalid.isEmpty => Left("Empty or blank MAC address.")
    case ValidAddress(address) => Right(address)
    case malformed => Left(s"Malformed MAC address: $malformed.")

  /**
   * Creates a MAC address from the specified string.
   *
   * @param string The string to validate as a MAC address.
   * @return The validated MAC address.
   */
  def make(string: String): IO[IllegalArgumentException, Address] =
    ZIO.fromEither(from(string)).mapError(IllegalArgumentException(_))