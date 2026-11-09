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
    test("validate and construct signal from dBm and names") {
      assertTrue(
        Signal.from(-20) == Right(Signal.Good),
        Signal.from(-60) == Right(Signal.Good),
        Signal.from(-61) == Right(Signal.Fair),
        Signal.from(-75) == Right(Signal.Fair),
        Signal.from(-76) == Right(Signal.Poor),
        Signal.from(-95) == Right(Signal.Poor),
        Signal.from(-96) == Right(Signal.Poor),
        Signal.from(-19) == Right(Signal.Good),
        Signal.from(0) == Right(Signal.Good),
        Signal.from("Good") == Right(Signal.Good),
        Signal.from("fair") == Right(Signal.Fair),
        Signal.from("  POOR  ") == Right(Signal.Poor),
        Signal.from("invalid") == Left("Invalid signal name: invalid.")
      )
    },
    test("construct signal effect with make") {
      for
        madeInt <- Signal.make(-50)
        madeName <- Signal.make("Good")
        err <- Signal.make("invalid").flip
      yield assertTrue(
        madeInt == Signal.Good,
        madeName == Signal.Good,
        err.getMessage == "Invalid signal name: invalid."
      )
    },
    test("encode and decode Signal as JSON") {
      val sig = Signal.Good
      val encoded = Encoder[Signal].apply(sig)
      assertTrue(
        encoded == Json.fromString("Good"),
        Decoder[Signal].decodeJson(encoded) == Right(Signal.Good),
        Decoder[Signal].decodeJson(Json.fromInt(-70)) == Right(Signal.Fair),
        Decoder[Signal].decodeJson(Json.fromInt(-85)) == Right(Signal.Poor),
        Decoder[Signal].decodeJson(Json.fromString("Fair")) == Right(Signal.Fair),
        Decoder[Signal].decodeJson(Json.fromString("Poor")) == Right(Signal.Poor),
        Decoder[Signal].decodeJson(Json.fromString("Unknown")).isLeft,
        Decoder[Signal].decodeJson(Json.arr()).isLeft
      )
    }
  )
