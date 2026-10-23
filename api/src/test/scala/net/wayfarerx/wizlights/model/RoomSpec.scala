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
 * Specifications for the Room model.
 */
object RoomSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("RoomSpec")(
    test("create valid room identifiers with from and make") {
      for
        fromOne <- ZIO.fromEither(Room.from(1))
        fromPositive <- ZIO.fromEither(Room.from(99))
        madeOne <- Room.make(1)
        madePositive <- Room.make(99)
      yield assertTrue(
        (fromOne: Int) == 1,
        (fromPositive: Int) == 99,
        (madeOne: Int) == 1,
        (madePositive: Int) == 99
      )
    },
    test("reject non-positive room identifiers") {
      val fromZero = Room.from(0)
      val fromNeg = Room.from(-1)
      for
        errZero <- Room.make(0).flip
        errNeg <- Room.make(-1).flip
      yield assertTrue(
        fromZero == Left("Invalid room ID: 0. Must be positive."),
        fromNeg == Left("Invalid room ID: -1. Must be positive."),
        errZero.isInstanceOf[IllegalArgumentException],
        errZero.getMessage == "Invalid room ID: 0. Must be positive.",
        errNeg.isInstanceOf[IllegalArgumentException],
        errNeg.getMessage == "Invalid room ID: -1. Must be positive."
      )
    },
    test("encode and decode Room as JSON") {
      for
        room <- Room.make(12)
        encoded = Encoder[Room].apply(room)
        decoded = Decoder[Room].decodeJson(encoded)
        invalidJson = Decoder[Room].decodeJson(Json.fromInt(-12))
        nonIntJson = Decoder[Room].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(12),
        decoded == Right(room),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
