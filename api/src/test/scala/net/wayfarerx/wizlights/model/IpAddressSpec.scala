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

import io.circe.{Decoder, Encoder, Json}

import zio.{Scope, ZIO}
import zio.test.*

/**
 * Specifications for the IpAddress model.
 */
object IpAddressSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("IpAddressSpec")(
    test("create valid IP addresses with from and make") {
      val ip0 = "0.0.0.0"
      val loopback = "127.0.0.1"
      val privateIp = "192.168.1.100"
      val maxIp = "255.255.255.255"
      val padded = "  10.0.0.1  "
      for
        from0 <- ZIO.fromEither(IpAddress.from(ip0))
        fromLoopback <- ZIO.fromEither(IpAddress.from(loopback))
        fromPrivate <- ZIO.fromEither(IpAddress.from(privateIp))
        fromMax <- ZIO.fromEither(IpAddress.from(maxIp))
        fromPadded <- ZIO.fromEither(IpAddress.from(padded))
        made0 <- IpAddress.make(ip0)
        madeLoopback <- IpAddress.make(loopback)
        madePrivate <- IpAddress.make(privateIp)
        madeMax <- IpAddress.make(maxIp)
        madePadded <- IpAddress.make(padded)
      yield assertTrue(
        from0 == "0.0.0.0",
        fromLoopback == "127.0.0.1",
        fromPrivate == "192.168.1.100",
        fromMax == "255.255.255.255",
        fromPadded == "10.0.0.1",
        made0 == "0.0.0.0",
        madeLoopback == "127.0.0.1",
        madePrivate == "192.168.1.100",
        madeMax == "255.255.255.255",
        madePadded == "10.0.0.1"
      )
    },
    test("reject empty or blank IP addresses") {
      val fromEmpty = IpAddress.from("")
      val fromBlank = IpAddress.from("   ")
      for
        makeEmpty <- IpAddress.make("").flip
        makeBlank <- IpAddress.make("   ").flip
      yield assertTrue(
        fromEmpty == Left("Empty or blank IP address."),
        fromBlank == Left("Empty or blank IP address."),
        makeEmpty.isInstanceOf[IllegalArgumentException],
        makeEmpty.getMessage == "Empty or blank IP address.",
        makeBlank.isInstanceOf[IllegalArgumentException],
        makeBlank.getMessage == "Empty or blank IP address."
      )
    },
    test("reject malformed IP addresses") {
      val outOfRange = "256.0.0.1"
      val tooFew = "192.168.1"
      val tooMany = "192.168.1.1.1"
      val letters = "192.168.a.1"
      val leadingDot = ".1.2.3.4"
      val trailingDot = "1.2.3.4."
      for
        errRange <- IpAddress.make(outOfRange).flip
        errFew <- IpAddress.make(tooFew).flip
        errMany <- IpAddress.make(tooMany).flip
        errLetters <- IpAddress.make(letters).flip
        errLeading <- IpAddress.make(leadingDot).flip
        errTrailing <- IpAddress.make(trailingDot).flip
      yield assertTrue(
        IpAddress.from(outOfRange).isLeft,
        IpAddress.from(tooFew).isLeft,
        IpAddress.from(tooMany).isLeft,
        IpAddress.from(letters).isLeft,
        IpAddress.from(leadingDot).isLeft,
        IpAddress.from(trailingDot).isLeft,
        errRange.getMessage == s"Malformed IP address: $outOfRange.",
        errFew.getMessage == s"Malformed IP address: $tooFew.",
        errMany.getMessage == s"Malformed IP address: $tooMany.",
        errLetters.getMessage == s"Malformed IP address: $letters.",
        errLeading.getMessage == s"Malformed IP address: $leadingDot.",
        errTrailing.getMessage == s"Malformed IP address: $trailingDot."
      )
    },
    test("encode and decode IpAddress as JSON") {
      for
        ip <- IpAddress.make("192.168.1.1")
        encoded = Encoder[IpAddress].apply(ip)
        decoded = Decoder[IpAddress].decodeJson(encoded)
        invalidJson = Decoder[IpAddress].decodeJson(Json.fromString("invalid-ip"))
        nonStringJson = Decoder[IpAddress].decodeJson(Json.fromInt(12345))
      yield assertTrue(
        encoded == Json.fromString("192.168.1.1"),
        decoded == Right(ip),
        invalidJson.isLeft,
        nonStringJson.isLeft
      )
    }
  )
