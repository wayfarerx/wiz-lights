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
    test("create valid module names with from and make") {
      val valid = "ESP32_WROOM"
      val padded = "  ESP32_DW01  "
      for
        fromValid <- ZIO.fromEither(Module.from(valid))
        fromPadded <- ZIO.fromEither(Module.from(padded))
        madeValid <- Module.make(valid)
        madePadded <- Module.make(padded)
      yield assertTrue(
        fromValid == "ESP32_WROOM",
        fromPadded == "ESP32_DW01",
        madeValid == "ESP32_WROOM",
        madePadded == "ESP32_DW01"
      )
    },
    test("reject empty or blank module names") {
      val fromEmpty = Module.from("")
      val fromBlank = Module.from("   ")
      for
        errEmpty <- Module.make("").flip
        errBlank <- Module.make("   ").flip
      yield assertTrue(
        fromEmpty == Left("Empty or blank module name."),
        fromBlank == Left("Empty or blank module name."),
        errEmpty.isInstanceOf[IllegalArgumentException],
        errEmpty.getMessage == "Empty or blank module name.",
        errBlank.isInstanceOf[IllegalArgumentException],
        errBlank.getMessage == "Empty or blank module name."
      )
    },
    test("encode and decode Module as JSON") {
      for
        mod <- Module.make("ESP32")
        encoded = Encoder[Module].apply(mod)
        decoded = Decoder[Module].decodeJson(encoded)
        invalidJson = Decoder[Module].decodeJson(Json.fromString("   "))
        nonStringJson = Decoder[Module].decodeJson(Json.fromInt(123))
      yield assertTrue(
        encoded == Json.fromString("ESP32"),
        decoded == Right(mod),
        invalidJson.isLeft,
        nonStringJson.isLeft
      )
    }
  )
