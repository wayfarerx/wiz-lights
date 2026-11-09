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
    test("validate and construct room identifiers from integers") {
      assertTrue(
        Room.from(1).map(r => (r: Int)) == Right(1),
        Room.from(99).map(r => (r: Int)) == Right(99),
        Room.from(0) == Left("Invalid room ID: 0. Must be positive."),
        Room.from(-1) == Left("Invalid room ID: -1. Must be positive.")
      )
    },
    test("construct room effect with make") {
      for
        made <- Room.make(1)
        err <- Room.make(0).flip
      yield assertTrue(
        (made: Int) == 1,
        err.getMessage == "Invalid room ID: 0. Must be positive."
      )
    },
    test("encode and decode Room as JSON") {
      val room = Room.from(12).toOption.get
      val encoded = Encoder[Room].apply(room)
      assertTrue(
        encoded == Json.fromInt(12),
        Decoder[Room].decodeJson(encoded) == Right(room),
        Decoder[Room].decodeJson(Json.fromInt(-12)).isLeft,
        Decoder[Room].decodeJson(Json.arr()).isLeft
      )
    }
  )
