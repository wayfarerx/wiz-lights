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
 * Specifications for the Speed model.
 */
object SpeedSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SpeedSpec")(
    test("predefined speed constants") {
      assertTrue(
        (Speed.Slowest: Int) == 10,
        (Speed.Fastest: Int) == 200,
        (Speed.Medium: Int) == 105
      )
    },
    test("create valid speed values with from and make") {
      for
        fromSlow <- ZIO.fromEither(Speed.from(10))
        fromMed <- ZIO.fromEither(Speed.from(105))
        fromFast <- ZIO.fromEither(Speed.from(200))
        fromCustom <- ZIO.fromEither(Speed.from(50))
        madeSlow <- Speed.make(10)
        madeMed <- Speed.make(105)
        madeFast <- Speed.make(200)
        madeCustom <- Speed.make(50)
      yield assertTrue(
        fromSlow == Speed.Slowest,
        fromMed == Speed.Medium,
        fromFast == Speed.Fastest,
        (fromCustom: Int) == 50,
        madeSlow == Speed.Slowest,
        madeMed == Speed.Medium,
        madeFast == Speed.Fastest,
        (madeCustom: Int) == 50
      )
    },
    test("reject speed values out of range") {
      val fromTooSlow = Speed.from(9)
      val fromTooFast = Speed.from(201)
      val fromNeg = Speed.from(-1)
      for
        errTooSlow <- Speed.make(9).flip
        errTooFast <- Speed.make(201).flip
        errNeg <- Speed.make(-1).flip
      yield assertTrue(
        fromTooSlow.isLeft,
        fromTooFast.isLeft,
        fromNeg.isLeft,
        errTooSlow.getMessage == "Invalid speed value: 9. Must be between 10 and 200.",
        errTooFast.getMessage == "Invalid speed value: 201. Must be between 10 and 200.",
        errNeg.getMessage == "Invalid speed value: -1. Must be between 10 and 200."
      )
    },
    test("encode and decode Speed as JSON") {
      for
        spd <- Speed.make(150)
        encoded = Encoder[Speed].apply(spd)
        decoded = Decoder[Speed].decodeJson(encoded)
        invalidJson = Decoder[Speed].decodeJson(Json.fromInt(300))
        nonIntJson = Decoder[Speed].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(150),
        decoded == Right(spd),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
