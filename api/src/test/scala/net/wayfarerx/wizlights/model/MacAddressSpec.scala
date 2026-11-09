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
    test("validate and construct MAC addresses from strings") {
      assertTrue(
        MacAddress.from("0123456789ab") == Right("0123456789ab"),
        MacAddress.from("A1B2C3D4E5F6") == Right("a1b2c3d4e5f6"),
        MacAddress.from("  0123456789ab  ") == Right("0123456789ab"),
        MacAddress.from("") == Left("Empty or blank MAC address."),
        MacAddress.from("   ") == Left("Empty or blank MAC address."),
        MacAddress.from("0123456789ag") == Left("Malformed MAC address: 0123456789ag."),
        MacAddress.from("0123456789a") == Left("Malformed MAC address: 0123456789a."),
        MacAddress.from("0123456789abcdef") == Left("Malformed MAC address: 0123456789abcdef."),
        MacAddress.from("01:23:45:67:89:ab") == Left("Malformed MAC address: 01:23:45:67:89:ab.")
      )
    },
    test("construct MAC address effect with make") {
      for
        made <- MacAddress.make("A1B2C3D4E5F6")
        errBlank <- MacAddress.make("   ").flip
        errMalformed <- MacAddress.make("bad").flip
      yield assertTrue(
        made == "a1b2c3d4e5f6",
        errBlank.getMessage == "Empty or blank MAC address.",
        errMalformed.getMessage == "Malformed MAC address: bad."
      )
    },
    test("encode and decode MacAddress as JSON") {
      val addr = MacAddress.from("0123456789ab").toOption.get
      val encoded = Encoder[MacAddress].apply(addr)
      assertTrue(
        encoded == Json.fromString("0123456789ab"),
        Decoder[MacAddress].decodeJson(encoded) == Right(addr),
        Decoder[MacAddress].decodeJson(Json.fromString("invalid")).isLeft,
        Decoder[MacAddress].decodeJson(Json.fromInt(123)).isLeft
      )
    }
  )
