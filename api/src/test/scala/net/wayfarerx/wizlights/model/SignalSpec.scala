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
 * Specifications for the Signal model.
 */
object SignalSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SignalSpec")(
    test("predefined signal constants") {
      assertTrue(
        Signal.Weakest == -95,
        Signal.Strongest == -20
      )
    },
    test("create valid signal values with from and make") {
      for
        fromStrong <- ZIO.fromEither(Signal.from(-20))
        fromGood <- ZIO.fromEither(Signal.from(-60))
        fromFairHigh <- ZIO.fromEither(Signal.from(-61))
        fromFairLow <- ZIO.fromEither(Signal.from(-75))
        fromPoorHigh <- ZIO.fromEither(Signal.from(-76))
        fromWeak <- ZIO.fromEither(Signal.from(-95))
        fromNameGood <- ZIO.fromEither(Signal.from("Good"))
        fromNameFair <- ZIO.fromEither(Signal.from("fair"))
        fromNamePoor <- ZIO.fromEither(Signal.from("  POOR  "))
        madeGood <- Signal.make(-50)
        madeFair <- Signal.make(-70)
        madePoor <- Signal.make(-90)
        madeNameGood <- Signal.make("Good")
        madeNameFair <- Signal.make("Fair")
        madeNamePoor <- Signal.make("Poor")
      yield assertTrue(
        fromStrong == Signal.Good,
        fromGood == Signal.Good,
        fromFairHigh == Signal.Fair,
        fromFairLow == Signal.Fair,
        fromPoorHigh == Signal.Poor,
        fromWeak == Signal.Poor,
        fromNameGood == Signal.Good,
        fromNameFair == Signal.Fair,
        fromNamePoor == Signal.Poor,
        madeGood == Signal.Good,
        madeFair == Signal.Fair,
        madePoor == Signal.Poor,
        madeNameGood == Signal.Good,
        madeNameFair == Signal.Fair,
        madeNamePoor == Signal.Poor
      )
    },
    test("classify signal values outside standard range and reject invalid names") {
      val fromTooWeak = Signal.from(-96)
      val fromTooStrong = Signal.from(-19)
      val fromZero = Signal.from(0)
      val fromInvalidName = Signal.from("invalid")
      for
        madeTooWeak <- Signal.make(-96)
        madeTooStrong <- Signal.make(-19)
        madeZero <- Signal.make(0)
        errInvalidName <- Signal.make("invalid").flip
      yield assertTrue(
        fromTooWeak == Right(Signal.Poor),
        fromTooStrong == Right(Signal.Good),
        fromZero == Right(Signal.Good),
        fromInvalidName.isLeft,
        madeTooWeak == Signal.Poor,
        madeTooStrong == Signal.Good,
        madeZero == Signal.Good,
        errInvalidName.getMessage == "Invalid signal name: invalid."
      )
    },
    test("encode and decode Signal as JSON") {
      for
        sigGood <- Signal.make(-45)
        sigFair <- Signal.make(-70)
        sigPoor <- Signal.make(-85)
        encodedGood = Encoder[Signal].apply(sigGood)
        decodedGood = Decoder[Signal].decodeJson(encodedGood)
        decodedFairFromInt = Decoder[Signal].decodeJson(Json.fromInt(-70))
        decodedPoorFromInt = Decoder[Signal].decodeJson(Json.fromInt(-85))
        decodedGoodFromString = Decoder[Signal].decodeJson(Json.fromString("Good"))
        decodedFairFromString = Decoder[Signal].decodeJson(Json.fromString("Fair"))
        decodedPoorFromString = Decoder[Signal].decodeJson(Json.fromString("Poor"))
        invalidString = Decoder[Signal].decodeJson(Json.fromString("Unknown"))
        nonIntOrStringJson = Decoder[Signal].decodeJson(Json.arr())
      yield assertTrue(
        encodedGood == Json.fromString("Good"),
        decodedGood == Right(Signal.Good),
        decodedFairFromInt == Right(Signal.Fair),
        decodedPoorFromInt == Right(Signal.Poor),
        decodedGoodFromString == Right(Signal.Good),
        decodedFairFromString == Right(Signal.Fair),
        decodedPoorFromString == Right(Signal.Poor),
        invalidString.isLeft,
        nonIntOrStringJson.isLeft
      )
    }
  )
