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
    test("validate and construct sources from strings") {
      assertTrue(
        Source.from("custom") == Right("custom"),
        Source.from("  custom_source  ") == Right("custom_source"),
        Source.from("") == Left("Empty or blank source."),
        Source.from("   ") == Left("Empty or blank source.")
      )
    },
    test("construct source effect with make") {
      for
        made <- Source.make("  custom  ")
        err <- Source.make("   ").flip
      yield assertTrue(
        made == "custom",
        err.getMessage == "Empty or blank source."
      )
    },
    test("encode and decode Source as JSON") {
      val src = Source.Udp
      val encoded = Encoder[Source].apply(src)
      assertTrue(
        encoded == Json.fromString("udp"),
        Decoder[Source].decodeJson(encoded) == Right(src),
        Decoder[Source].decodeJson(Json.fromString("   ")).isLeft,
        Decoder[Source].decodeJson(Json.fromInt(1)).isLeft
      )
    }
  )
