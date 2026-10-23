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
        (Signal.Weakest: Int) == -95,
        (Signal.Strongest: Int) == -20,
        (Signal.Medium: Int) == (Signal.Strongest - Signal.Weakest) / 2 + Signal.Weakest
      )
    },
    test("create valid signal values with from and make") {
      for
        fromWeak <- ZIO.fromEither(Signal.from(-95))
        fromMid <- ZIO.fromEither(Signal.from(-50))
        fromStrong <- ZIO.fromEither(Signal.from(-20))
        madeWeak <- Signal.make(-95)
        madeMid <- Signal.make(-50)
        madeStrong <- Signal.make(-20)
      yield assertTrue(
        fromWeak == Signal.Weakest,
        fromStrong == Signal.Strongest,
        (fromMid: Int) == -50,
        madeWeak == Signal.Weakest,
        madeStrong == Signal.Strongest,
        (madeMid: Int) == -50
      )
    },
    test("reject signal values out of range") {
      val fromTooWeak = Signal.from(-96)
      val fromTooStrong = Signal.from(-19)
      val fromZero = Signal.from(0)
      for
        errTooWeak <- Signal.make(-96).flip
        errTooStrong <- Signal.make(-19).flip
        errZero <- Signal.make(0).flip
      yield assertTrue(
        fromTooWeak.isLeft,
        fromTooStrong.isLeft,
        fromZero.isLeft,
        errTooWeak.getMessage == "Invalid signal value: -96. Must be between -95 and -20.",
        errTooStrong.getMessage == "Invalid signal value: -19. Must be between -95 and -20.",
        errZero.getMessage == "Invalid signal value: 0. Must be between -95 and -20."
      )
    },
    test("encode and decode Signal as JSON") {
      for
        sig <- Signal.make(-45)
        encoded = Encoder[Signal].apply(sig)
        decoded = Decoder[Signal].decodeJson(encoded)
        invalidJson = Decoder[Signal].decodeJson(Json.fromInt(-10))
        nonIntJson = Decoder[Signal].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(-45),
        decoded == Right(sig),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
