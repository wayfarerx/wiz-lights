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
 * Specifications for the Timestamp model.
 */
object TimestampSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TimestampSpec")(
    test("create valid timestamps with from and make") {
      for
        fromZero <- ZIO.fromEither(Timestamp.from(0L))
        fromPositive <- ZIO.fromEither(Timestamp.from(1700000000000L))
        madeZero <- Timestamp.make(0L)
        madePositive <- Timestamp.make(1700000000000L)
      yield assertTrue(
        (fromZero: Long) == 0L,
        (fromPositive: Long) == 1700000000000L,
        (madeZero: Long) == 0L,
        (madePositive: Long) == 1700000000000L
      )
    },
    test("reject negative timestamps") {
      val fromNeg = Timestamp.from(-1L)
      for
        errNeg <- Timestamp.make(-1L).flip
      yield assertTrue(
        fromNeg == Left("Invalid timestamp: -1. Cannot be negative."),
        errNeg.isInstanceOf[IllegalArgumentException],
        errNeg.getMessage == "Invalid timestamp: -1. Cannot be negative."
      )
    },
    test("encode and decode Timestamp as JSON") {
      for
        timestamp <- Timestamp.make(1700000000000L)
        encoded = Encoder[Timestamp].apply(timestamp)
        decoded = Decoder[Timestamp].decodeJson(encoded)
        invalidJson = Decoder[Timestamp].decodeJson(Json.fromLong(-1L))
        nonLongJson = Decoder[Timestamp].decodeJson(Json.arr())
      yield assertTrue(
        encoded == Json.fromLong(1700000000000L),
        decoded == Right(timestamp),
        invalidJson.isLeft,
        nonLongJson.isLeft
      )
    }
  )
