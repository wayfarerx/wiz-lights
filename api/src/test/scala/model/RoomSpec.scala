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

import cats.kernel.Order
import zio.Scope
import zio.test.*

/**
 * Specifications for the Room model.
 */
object RoomSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("RoomSpec")(
    test("successfully create valid room IDs") {
      for
        one <- Room.make(1)
        custom <- Room.make(42)
      yield assertTrue(
        (one: Int) == 1,
        (custom: Int) == 42
      )
    },
    test("fail when room ID is not positive") {
      for
        errorZero <- Room.make(0).flip
        errorNegative <- Room.make(-1).flip
      yield assertTrue(
        errorZero.isInstanceOf[IllegalArgumentException],
        errorZero.getMessage == "Invalid room ID: 0. Must be positive.",
        errorNegative.isInstanceOf[IllegalArgumentException],
        errorNegative.getMessage == "Invalid room ID: -1. Must be positive."
      )
    },
    test("support ordering and cats order") {
      for
        room1 <- Room.make(1)
        room2 <- Room.make(2)
      yield assertTrue(
        Ordering[Room].compare(room1, room2) < 0,
        Ordering[Room].compare(room2, room1) > 0,
        Ordering[Room].compare(room1, room1) == 0,
        Order[Room].compare(room1, room2) < 0,
        Order[Room].eqv(room1, room1)
      )
    }
  )
