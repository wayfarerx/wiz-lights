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
 * Specifications for the Temperature model.
 */
object TemperatureSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TemperatureSpec")(
    test("predefined temperature constants") {
      assertTrue(
        (Temperature.Warmest: Int) == 2200,
        (Temperature.Coolest: Int) == 6500,
        (Temperature.Medium: Int) == 4350
      )
    },
    test("validate and construct temperature from integers") {
      assertTrue(
        Temperature.from(2200) == Right(Temperature.Warmest),
        Temperature.from(4350) == Right(Temperature.Medium),
        Temperature.from(6500) == Right(Temperature.Coolest),
        Temperature.from(3000).map(t => (t: Int)) == Right(3000),
        Temperature.from(2199) == Left("Invalid temperature value: 2199. Must be between 2200 and 6500."),
        Temperature.from(6501) == Left("Invalid temperature value: 6501. Must be between 2200 and 6500."),
        Temperature.from(0) == Left("Invalid temperature value: 0. Must be between 2200 and 6500."),
        Temperature.from(-100) == Left("Invalid temperature value: -100. Must be between 2200 and 6500.")
      )
    },
    test("construct temperature effect with make") {
      for
        made <- Temperature.make(4350)
        err <- Temperature.make(2199).flip
      yield assertTrue(
        made == Temperature.Medium,
        err.getMessage == "Invalid temperature value: 2199. Must be between 2200 and 6500."
      )
    },
    test("encode and decode Temperature as JSON") {
      val temp = Temperature.Medium
      val encoded = Encoder[Temperature].apply(temp)
      assertTrue(
        encoded == Json.fromInt(4350),
        Decoder[Temperature].decodeJson(encoded) == Right(temp),
        Decoder[Temperature].decodeJson(Json.fromInt(7000)).isLeft,
        Decoder[Temperature].decodeJson(Json.arr()).isLeft
      )
    }
  )
