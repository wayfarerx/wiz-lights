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
    test("create valid temperature values with from and make") {
      for
        fromWarm <- ZIO.fromEither(Temperature.from(2200))
        fromMed <- ZIO.fromEither(Temperature.from(4350))
        fromCool <- ZIO.fromEither(Temperature.from(6500))
        fromCustom <- ZIO.fromEither(Temperature.from(3000))
        madeWarm <- Temperature.make(2200)
        madeMed <- Temperature.make(4350)
        madeCool <- Temperature.make(6500)
        madeCustom <- Temperature.make(3000)
      yield assertTrue(
        fromWarm == Temperature.Warmest,
        fromMed == Temperature.Medium,
        fromCool == Temperature.Coolest,
        (fromCustom: Int) == 3000,
        madeWarm == Temperature.Warmest,
        madeMed == Temperature.Medium,
        madeCool == Temperature.Coolest,
        (madeCustom: Int) == 3000
      )
    },
    test("reject temperature values out of range") {
      val fromTooWarm = Temperature.from(2199)
      val fromTooCool = Temperature.from(6501)
      val fromZero = Temperature.from(0)
      val fromNeg = Temperature.from(-100)
      for
        errTooWarm <- Temperature.make(2199).flip
        errTooCool <- Temperature.make(6501).flip
        errZero <- Temperature.make(0).flip
        errNeg <- Temperature.make(-100).flip
      yield assertTrue(
        fromTooWarm.isLeft,
        fromTooCool.isLeft,
        fromZero.isLeft,
        fromNeg.isLeft,
        errTooWarm.getMessage == "Invalid temperature value: 2199. Must be between 2200 and 6500.",
        errTooCool.getMessage == "Invalid temperature value: 6501. Must be between 2200 and 6500.",
        errZero.getMessage == "Invalid temperature value: 0. Must be between 2200 and 6500.",
        errNeg.getMessage == "Invalid temperature value: -100. Must be between 2200 and 6500."
      )
    },
    test("encode and decode Temperature as JSON") {
      for
        temp <- Temperature.make(5000)
        encoded = Encoder[Temperature].apply(temp)
        decoded = Decoder[Temperature].decodeJson(encoded)
        invalidJson = Decoder[Temperature].decodeJson(Json.fromInt(7000))
        nonIntJson = Decoder[Temperature].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(5000),
        decoded == Right(temp),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
