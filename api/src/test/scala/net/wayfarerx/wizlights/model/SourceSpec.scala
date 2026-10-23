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
 * Specifications for the Source model.
 */
object SourceSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SourceSpec")(
    test("predefined source constants") {
      assertTrue(
        (Source.Heartbeat: String) == "hb",
        (Source.Udp: String) == "udp",
        (Source.App: String) == "wfa",
        (Source.Remote: String) == "wzm",
        (Source.Sensor: String) == "pir"
      )
    },
    test("create valid sources with from and make") {
      val valid = "custom"
      val padded = "  custom_source  "
      for
        fromValid <- ZIO.fromEither(Source.from(valid))
        fromPadded <- ZIO.fromEither(Source.from(padded))
        madeValid <- Source.make(valid)
        madePadded <- Source.make(padded)
      yield assertTrue(
        fromValid == "custom",
        fromPadded == "custom_source",
        madeValid == "custom",
        madePadded == "custom_source"
      )
    },
    test("reject empty or blank sources") {
      val fromEmpty = Source.from("")
      val fromBlank = Source.from("   ")
      for
        errEmpty <- Source.make("").flip
        errBlank <- Source.make("   ").flip
      yield assertTrue(
        fromEmpty == Left("Empty or blank source."),
        fromBlank == Left("Empty or blank source."),
        errEmpty.isInstanceOf[IllegalArgumentException],
        errEmpty.getMessage == "Empty or blank source.",
        errBlank.isInstanceOf[IllegalArgumentException],
        errBlank.getMessage == "Empty or blank source."
      )
    },
    test("encode and decode Source as JSON") {
      for
        src <- Source.make("udp")
        encoded = Encoder[Source].apply(src)
        decoded = Decoder[Source].decodeJson(encoded)
        invalidJson = Decoder[Source].decodeJson(Json.fromString("   "))
        nonStringJson = Decoder[Source].decodeJson(Json.fromInt(1))
      yield assertTrue(
        encoded == Json.fromString("udp"),
        decoded == Right(src),
        invalidJson.isLeft,
        nonStringJson.isLeft
      )
    }
  )
