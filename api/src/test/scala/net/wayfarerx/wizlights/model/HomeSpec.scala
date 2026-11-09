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
    test("validate and construct home identifiers from integers") {
      assertTrue(
        Home.from(1).map(h => (h: Int)) == Right(1),
        Home.from(12345).map(h => (h: Int)) == Right(12345),
        Home.from(0) == Left("Invalid home ID: 0. Must be positive."),
        Home.from(-10) == Left("Invalid home ID: -10. Must be positive.")
      )
    },
    test("construct home effect with make") {
      for
        made <- Home.make(1)
        err <- Home.make(0).flip
      yield assertTrue(
        (made: Int) == 1,
        err.getMessage == "Invalid home ID: 0. Must be positive."
      )
    },
    test("encode and decode Home as JSON") {
      val home = Home.from(42).toOption.get
      val encoded = Encoder[Home].apply(home)
      assertTrue(
        encoded == Json.fromInt(42),
        Decoder[Home].decodeJson(encoded) == Right(home),
        Decoder[Home].decodeJson(Json.fromInt(-1)).isLeft,
        Decoder[Home].decodeJson(Json.arr()).isLeft
      )
    }
  )
