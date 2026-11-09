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
 * Specifications for the Module model.
 */
object ModuleSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ModuleSpec")(
    test("validate and construct module names from strings") {
      assertTrue(
        Module.from("ESP32_WROOM") == Right("ESP32_WROOM"),
        Module.from("  ESP32_DW01  ") == Right("ESP32_DW01"),
        Module.from("") == Left("Empty or blank module name."),
        Module.from("   ") == Left("Empty or blank module name.")
      )
    },
    test("construct module effect with make") {
      for
        made <- Module.make("  ESP32_WROOM  ")
        err <- Module.make("   ").flip
      yield assertTrue(
        made == "ESP32_WROOM",
        err.getMessage == "Empty or blank module name."
      )
    },
    test("encode and decode Module as JSON") {
      val mod = Module.from("ESP32").toOption.get
      val encoded = Encoder[Module].apply(mod)
      assertTrue(
        encoded == Json.fromString("ESP32"),
        Decoder[Module].decodeJson(encoded) == Right(mod),
        Decoder[Module].decodeJson(Json.fromString("   ")).isLeft,
        Decoder[Module].decodeJson(Json.fromInt(123)).isLeft
      )
    }
  )
