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
 * Specifications for the Home model.
 */
object HomeSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("HomeSpec")(
    test("successfully create valid home IDs") {
      for
        one <- Home.make(1)
        custom <- Home.make(789012)
      yield assertTrue(
        (one: Int) == 1,
        (custom: Int) == 789012
      )
    },
    test("fail when home ID is not positive") {
      for
        errorZero <- Home.make(0).flip
        errorNegative <- Home.make(-1).flip
      yield assertTrue(
        errorZero.isInstanceOf[IllegalArgumentException],
        errorZero.getMessage == "Invalid home ID: 0. Must be positive.",
        errorNegative.isInstanceOf[IllegalArgumentException],
        errorNegative.getMessage == "Invalid home ID: -1. Must be positive."
      )
    },
    test("support ordering and cats order") {
      for
        home1 <- Home.make(100)
        home2 <- Home.make(200)
      yield assertTrue(
        Ordering[Home].compare(home1, home2) < 0,
        Ordering[Home].compare(home2, home1) > 0,
        Ordering[Home].compare(home1, home1) == 0,
        Order[Home].compare(home1, home2) < 0,
        Order[Home].eqv(home1, home1)
      )
    }
  )
