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
    test("validate and construct firmware versions from strings") {
      assertTrue(
        Firmware.from("1.25.0") == Right("1.25.0"),
        Firmware.from("  v2.0.1  ") == Right("v2.0.1"),
        Firmware.from("") == Left("Empty or blank firmware version."),
        Firmware.from("   ") == Left("Empty or blank firmware version.")
      )
    },
    test("construct firmware effect with make") {
      for
        made <- Firmware.make("  1.25.0  ")
        err <- Firmware.make("   ").flip
      yield assertTrue(
        made == "1.25.0",
        err.getMessage == "Empty or blank firmware version."
      )
    },
    test("encode and decode Firmware as JSON") {
      val fw = Firmware.from("1.25.0").toOption.get
      val encoded = Encoder[Firmware].apply(fw)
      assertTrue(
        encoded == Json.fromString("1.25.0"),
        Decoder[Firmware].decodeJson(encoded) == Right(fw),
        Decoder[Firmware].decodeJson(Json.fromString("   ")).isLeft,
        Decoder[Firmware].decodeJson(Json.fromInt(125)).isLeft
      )
    }
  )
