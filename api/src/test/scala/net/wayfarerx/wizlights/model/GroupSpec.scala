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
    test("create valid group identifiers with from and make") {
      for
        fromOne <- ZIO.fromEither(Group.from(1))
        fromPositive <- ZIO.fromEither(Group.from(100))
        madeOne <- Group.make(1)
        madePositive <- Group.make(100)
      yield assertTrue(
        (fromOne: Int) == 1,
        (fromPositive: Int) == 100,
        (madeOne: Int) == 1,
        (madePositive: Int) == 100
      )
    },
    test("reject non-positive group identifiers") {
      val fromZero = Group.from(0)
      val fromNeg = Group.from(-5)
      for
        errZero <- Group.make(0).flip
        errNeg <- Group.make(-5).flip
      yield assertTrue(
        fromZero == Left("Invalid group ID: 0. Must be positive."),
        fromNeg == Left("Invalid group ID: -5. Must be positive."),
        errZero.isInstanceOf[IllegalArgumentException],
        errZero.getMessage == "Invalid group ID: 0. Must be positive.",
        errNeg.isInstanceOf[IllegalArgumentException],
        errNeg.getMessage == "Invalid group ID: -5. Must be positive."
      )
    },
    test("encode and decode Group as JSON") {
      for
        group <- Group.make(3)
        encoded = Encoder[Group].apply(group)
        decoded = Decoder[Group].decodeJson(encoded)
        invalidJson = Decoder[Group].decodeJson(Json.fromInt(0))
        nonIntJson = Decoder[Group].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(3),
        decoded == Right(group),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
