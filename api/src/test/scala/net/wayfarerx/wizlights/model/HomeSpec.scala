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
 * Specifications for the Home model.
 */
object HomeSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("HomeSpec")(
    test("create valid home identifiers with from and make") {
      for
        fromOne <- ZIO.fromEither(Home.from(1))
        fromPositive <- ZIO.fromEither(Home.from(12345))
        madeOne <- Home.make(1)
        madePositive <- Home.make(12345)
      yield assertTrue(
        (fromOne: Int) == 1,
        (fromPositive: Int) == 12345,
        (madeOne: Int) == 1,
        (madePositive: Int) == 12345
      )
    },
    test("reject non-positive home identifiers") {
      val fromZero = Home.from(0)
      val fromNeg = Home.from(-10)
      for
        errZero <- Home.make(0).flip
        errNeg <- Home.make(-10).flip
      yield assertTrue(
        fromZero == Left("Invalid home ID: 0. Must be positive."),
        fromNeg == Left("Invalid home ID: -10. Must be positive."),
        errZero.isInstanceOf[IllegalArgumentException],
        errZero.getMessage == "Invalid home ID: 0. Must be positive.",
        errNeg.isInstanceOf[IllegalArgumentException],
        errNeg.getMessage == "Invalid home ID: -10. Must be positive."
      )
    },
    test("encode and decode Home as JSON") {
      for
        home <- Home.make(42)
        encoded = Encoder[Home].apply(home)
        decoded = Decoder[Home].decodeJson(encoded)
        invalidJson = Decoder[Home].decodeJson(Json.fromInt(-1))
        nonIntJson = Decoder[Home].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(42),
        decoded == Right(home),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
