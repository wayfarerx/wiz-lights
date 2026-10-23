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
 * Specifications for the MacAddress model.
 */
object MacAddressSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("MacAddressSpec")(
    test("create valid MAC addresses with from and make") {
      val raw = "0123456789ab"
      val upper = "A1B2C3D4E5F6"
      val padded = "  0123456789ab  "
      for
        fromValid <- ZIO.fromEither(MacAddress.from(raw))
        fromUpper <- ZIO.fromEither(MacAddress.from(upper))
        fromPadded <- ZIO.fromEither(MacAddress.from(padded))
        madeValid <- MacAddress.make(raw)
        madeUpper <- MacAddress.make(upper)
        madePadded <- MacAddress.make(padded)
      yield assertTrue(
        fromValid == "0123456789ab",
        fromUpper == "a1b2c3d4e5f6",
        fromPadded == "0123456789ab",
        madeValid == "0123456789ab",
        madeUpper == "a1b2c3d4e5f6",
        madePadded == "0123456789ab"
      )
    },
    test("reject empty or blank MAC addresses") {
      val fromEmpty = MacAddress.from("")
      val fromBlank = MacAddress.from("   ")
      for
        makeEmpty <- MacAddress.make("").flip
        makeBlank <- MacAddress.make("   ").flip
      yield assertTrue(
        fromEmpty == Left("Empty or blank MAC address."),
        fromBlank == Left("Empty or blank MAC address."),
        makeEmpty.isInstanceOf[IllegalArgumentException],
        makeEmpty.getMessage == "Empty or blank MAC address.",
        makeBlank.isInstanceOf[IllegalArgumentException],
        makeBlank.getMessage == "Empty or blank MAC address."
      )
    },
    test("reject malformed MAC addresses") {
      val invalidChars = "0123456789ag"
      val tooShort = "0123456789a"
      val tooLong = "0123456789abcdef"
      val withColons = "01:23:45:67:89:ab"
      for
        errChars <- MacAddress.make(invalidChars).flip
        errShort <- MacAddress.make(tooShort).flip
        errLong <- MacAddress.make(tooLong).flip
        errColons <- MacAddress.make(withColons).flip
      yield assertTrue(
        MacAddress.from(invalidChars).isLeft,
        MacAddress.from(tooShort).isLeft,
        MacAddress.from(tooLong).isLeft,
        MacAddress.from(withColons).isLeft,
        errChars.getMessage == s"Malformed MAC address: $invalidChars.",
        errShort.getMessage == s"Malformed MAC address: $tooShort.",
        errLong.getMessage == s"Malformed MAC address: $tooLong.",
        errColons.getMessage == s"Malformed MAC address: $withColons."
      )
    },
    test("encode and decode MacAddress as JSON") {
      for
        addr <- MacAddress.make("0123456789ab")
        encoded = Encoder[MacAddress].apply(addr)
        decoded = Decoder[MacAddress].decodeJson(encoded)
        invalidJson = Decoder[MacAddress].decodeJson(Json.fromString("invalid"))
        nonStringJson = Decoder[MacAddress].decodeJson(Json.fromInt(123))
      yield assertTrue(
        encoded == Json.fromString("0123456789ab"),
        decoded == Right(addr),
        invalidJson.isLeft,
        nonStringJson.isLeft
      )
    }
  )
