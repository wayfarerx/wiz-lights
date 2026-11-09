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
    test("validate and construct speed from integers") {
      assertTrue(
        Speed.from(10) == Right(Speed.Slowest),
        Speed.from(105) == Right(Speed.Medium),
        Speed.from(200) == Right(Speed.Fastest),
        Speed.from(50).map(s => (s: Int)) == Right(50),
        Speed.from(9) == Left("Invalid speed value: 9. Must be between 10 and 200."),
        Speed.from(201) == Left("Invalid speed value: 201. Must be between 10 and 200."),
        Speed.from(-1) == Left("Invalid speed value: -1. Must be between 10 and 200.")
      )
    },
    test("construct speed effect with make") {
      for
        made <- Speed.make(105)
        err <- Speed.make(9).flip
      yield assertTrue(
        made == Speed.Medium,
        err.getMessage == "Invalid speed value: 9. Must be between 10 and 200."
      )
    },
    test("encode and decode Speed as JSON") {
      val spd = Speed.Medium
      val encoded = Encoder[Speed].apply(spd)
      assertTrue(
        encoded == Json.fromInt(105),
        Decoder[Speed].decodeJson(encoded) == Right(spd),
        Decoder[Speed].decodeJson(Json.fromInt(300)).isLeft,
        Decoder[Speed].decodeJson(Json.arr()).isLeft
      )
    }
  )
