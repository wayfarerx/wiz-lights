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
 * Specifications for the Group model.
 */
object GroupSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("GroupSpec")(
    test("validate and construct group identifiers from integers") {
      assertTrue(
        Group.from(1).map(g => (g: Int)) == Right(1),
        Group.from(100).map(g => (g: Int)) == Right(100),
        Group.from(0) == Left("Invalid group ID: 0. Must be positive."),
        Group.from(-5) == Left("Invalid group ID: -5. Must be positive.")
      )
    },
    test("construct group effect with make") {
      for
        made <- Group.make(1)
        err <- Group.make(0).flip
      yield assertTrue(
        (made: Int) == 1,
        err.getMessage == "Invalid group ID: 0. Must be positive."
      )
    },
    test("encode and decode Group as JSON") {
      val group = Group.from(3).toOption.get
      val encoded = Encoder[Group].apply(group)
      assertTrue(
        encoded == Json.fromInt(3),
        Decoder[Group].decodeJson(encoded) == Right(group),
        Decoder[Group].decodeJson(Json.fromInt(0)).isLeft,
        Decoder[Group].decodeJson(Json.arr()).isLeft
      )
    }
  )
