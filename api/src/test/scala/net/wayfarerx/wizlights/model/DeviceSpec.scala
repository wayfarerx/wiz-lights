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
 * Specifications for the Device model.
 */
object DeviceSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("DeviceSpec")(
    test("create valid device identifiers with from and make") {
      for
        fromZero <- ZIO.fromEither(Device.from(0))
        fromPositive <- ZIO.fromEither(Device.from(42))
        madeZero <- Device.make(0)
        madePositive <- Device.make(42)
      yield assertTrue(
        (fromZero: Int) == 0,
        (fromPositive: Int) == 42,
        (madeZero: Int) == 0,
        (madePositive: Int) == 42
      )
    },
    test("reject negative device identifiers") {
      val fromNeg = Device.from(-1)
      for
        errNeg <- Device.make(-1).flip
      yield assertTrue(
        fromNeg == Left("Invalid device type ID: -1. Cannot be negative."),
        errNeg.isInstanceOf[IllegalArgumentException],
        errNeg.getMessage == "Invalid device type ID: -1. Cannot be negative."
      )
    },
    test("encode and decode Device as JSON") {
      for
        device <- Device.make(5)
        encoded = Encoder[Device].apply(device)
        decoded = Decoder[Device].decodeJson(encoded)
        invalidJson = Decoder[Device].decodeJson(Json.fromInt(-5))
        nonIntJson = Decoder[Device].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromInt(5),
        decoded == Right(device),
        invalidJson.isLeft,
        nonIntJson.isLeft
      )
    }
  )
