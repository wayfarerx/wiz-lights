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
 * Specifications for the Brightness model.
 */
object BrightnessSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BrightnessSpec")(
    test("predefined brightness constants") {
      assertTrue(
        (Brightness.Dimmest: Int) == 10,
        (Brightness.Brightest: Int) == 100,
        (Brightness.Medium: Int) == 55
      )
    },
    test("validate and construct brightness from integers") {
      assertTrue(
        Brightness.from(10) == Right(Brightness.Dimmest),
        Brightness.from(55) == Right(Brightness.Medium),
        Brightness.from(100) == Right(Brightness.Brightest),
        Brightness.from(75).map(b => (b: Int)) == Right(75),
        Brightness.from(9) == Left("Invalid brightness value: 9. Must be between 10 and 100."),
        Brightness.from(101) == Left("Invalid brightness value: 101. Must be between 10 and 100."),
        Brightness.from(-1) == Left("Invalid brightness value: -1. Must be between 10 and 100.")
      )
    },
    test("construct brightness effect with make") {
      for
        made <- Brightness.make(55)
        err <- Brightness.make(9).flip
      yield assertTrue(
        made == Brightness.Medium,
        err.getMessage == "Invalid brightness value: 9. Must be between 10 and 100."
      )
    },
    test("encode and decode Brightness as JSON") {
      val b = Brightness.Medium
      val encoded = Encoder[Brightness].apply(b)
      assertTrue(
        encoded == Json.fromInt(55),
        Decoder[Brightness].decodeJson(encoded) == Right(b),
        Decoder[Brightness].decodeJson(Json.fromInt(150)).isLeft,
        Decoder[Brightness].decodeJson(Json.arr()).isLeft
      )
    }
  )
