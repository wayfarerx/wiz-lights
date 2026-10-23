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
    test("create valid brightness values with from and make") {
      for
        fromDim <- ZIO.fromEither(Brightness.from(10))
        fromMed <- ZIO.fromEither(Brightness.from(55))
        fromBright <- ZIO.fromEither(Brightness.from(100))
        fromCustom <- ZIO.fromEither(Brightness.from(75))
        madeDim <- Brightness.make(10)
        madeMed <- Brightness.make(55)
        madeBright <- Brightness.make(100)
        madeCustom <- Brightness.make(75)
      yield assertTrue(
        fromDim == Brightness.Dimmest,
        fromMed == Brightness.Medium,
        fromBright == Brightness.Brightest,
        (fromCustom: Int) == 75,
        madeDim == Brightness.Dimmest,
        madeMed == Brightness.Medium,
        madeBright == Brightness.Brightest,
        (madeCustom: Int) == 75
      )
    },
    test("reject brightness out of range") {
      val fromTooLow = Brightness.from(9)
      val fromTooHigh = Brightness.from(101)
      val fromNegative = Brightness.from(-1)
      for
        errTooLow <- Brightness.make(9).flip
        errTooHigh <- Brightness.make(101).flip
        errNegative <- Brightness.make(-1).flip
      yield assertTrue(
        fromTooLow.isLeft,
        fromTooHigh.isLeft,
        fromNegative.isLeft,
        errTooLow.getMessage == "Invalid brightness value: 9. Must be between 10 and 100.",
        errTooHigh.getMessage == "Invalid brightness value: 101. Must be between 10 and 100.",
        errNegative.getMessage == "Invalid brightness value: -1. Must be between 10 and 100."
      )
    },
    test("encode and decode Brightness as JSON") {
      for
        b <- Brightness.make(70)
        encoded = Encoder[Brightness].apply(b)
        decoded = Decoder[Brightness].decodeJson(encoded)
        invalidJson = Decoder[Brightness].decodeJson(Json.fromInt(150))
        nonIntJson = Decoder[Brightness].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(70),
        decoded == Right(b),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
