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
    test("validate and construct device identifiers from integers") {
      assertTrue(
        Device.from(0).map(d => (d: Int)) == Right(0),
        Device.from(42).map(d => (d: Int)) == Right(42),
        Device.from(-1) == Left("Invalid device type ID: -1. Cannot be negative.")
      )
    },
    test("construct device effect with make") {
      for
        made <- Device.make(42)
        err <- Device.make(-1).flip
      yield assertTrue(
        (made: Int) == 42,
        err.getMessage == "Invalid device type ID: -1. Cannot be negative."
      )
    },
    test("encode and decode Device as JSON") {
      val device = Device.from(42).toOption.get
      val encoded = Encoder[Device].apply(device)
      assertTrue(
        encoded == Json.fromInt(42),
        Decoder[Device].decodeJson(encoded) == Right(device),
        Decoder[Device].decodeJson(Json.fromInt(-1)).isLeft,
        Decoder[Device].decodeJson(Json.arr()).isLeft
      )
    }
  )
