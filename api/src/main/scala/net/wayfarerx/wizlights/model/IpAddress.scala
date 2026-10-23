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

/** An IPv4 address. */
opaque type IpAddress <: String = String

/**
 * Factory for IP addresses.
 */
object IpAddress:

  /** The circe encoder for IP addresses. */
  given Encoder[IpAddress] = Encoder.encodeString

  /** The circe decoder for IP addresses. */
  given Decoder[IpAddress] = Decoder.decodeString.emap(from)

  /** A pattern matching a single byte in decimal notation (0 - 255). */
  private inline val Octet = """(?:25[0-5]|2[0-4]\d|1\d{2}|[1-9]?\d)"""

  /** A pattern that matches valid IP address strings. */
  private val ValidIpAddress = s"((?:$Octet\\.){3}$Octet)".r

  /**
   * Attempts to create an IP address from the specified string.
   *
   * @param string The string to validate as an IP address.
   * @return The validated IP address or a failure.
   */
  def from(string: String): Either[String, IpAddress] = string.trim match
    case invalid if invalid.isEmpty => Left("Empty or blank IP address.")
    case ValidIpAddress(ipAddress) => Right(ipAddress)
    case malformed => Left(s"Malformed IP address: $malformed.")

  /**
   * Creates an IP address from the specified string.
   *
   * @param string The string to validate as an IP address.
   * @return The validated IP address.
   */
  def make(string: String): IO[IllegalArgumentException, IpAddress] =
    ZIO.fromEither(from(string)).mapError(IllegalArgumentException(_))
