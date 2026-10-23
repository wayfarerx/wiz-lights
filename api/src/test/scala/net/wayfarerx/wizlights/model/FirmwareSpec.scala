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
 * Specifications for the Firmware model.
 */
object FirmwareSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("FirmwareSpec")(
    test("create valid firmware versions with from and make") {
      val valid = "1.25.0"
      val padded = "  v2.0.1  "
      for
        fromValid <- ZIO.fromEither(Firmware.from(valid))
        fromPadded <- ZIO.fromEither(Firmware.from(padded))
        madeValid <- Firmware.make(valid)
        madePadded <- Firmware.make(padded)
      yield assertTrue(
        fromValid == "1.25.0",
        fromPadded == "v2.0.1",
        madeValid == "1.25.0",
        madePadded == "v2.0.1"
      )
    },
    test("reject empty or blank firmware versions") {
      val fromEmpty = Firmware.from("")
      val fromBlank = Firmware.from("   ")
      for
        errEmpty <- Firmware.make("").flip
        errBlank <- Firmware.make("   ").flip
      yield assertTrue(
        fromEmpty == Left("Empty or blank firmware version."),
        fromBlank == Left("Empty or blank firmware version."),
        errEmpty.isInstanceOf[IllegalArgumentException],
        errEmpty.getMessage == "Empty or blank firmware version.",
        errBlank.isInstanceOf[IllegalArgumentException],
        errBlank.getMessage == "Empty or blank firmware version."
      )
    },
    test("encode and decode Firmware as JSON") {
      for
        fw <- Firmware.make("1.25.0")
        encoded = Encoder[Firmware].apply(fw)
        decoded = Decoder[Firmware].decodeJson(encoded)
        invalidJson = Decoder[Firmware].decodeJson(Json.fromString("   "))
        nonStringJson = Decoder[Firmware].decodeJson(Json.fromInt(125))
      yield assertTrue(
        encoded == Json.fromString("1.25.0"),
        decoded == Right(fw),
        invalidJson.isLeft,
        nonStringJson.isLeft
      )
    }
  )
