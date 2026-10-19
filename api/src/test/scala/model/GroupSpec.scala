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
 * Specifications for the Group model.
 */
object GroupSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("GroupSpec")(
    test("successfully create valid group IDs") {
      for
        one <- Group.make(1)
        custom <- Group.make(5)
      yield assertTrue(
        (one: Int) == 1,
        (custom: Int) == 5
      )
    },
    test("fail when group ID is not positive") {
      for
        errorZero <- Group.make(0).flip
        errorNegative <- Group.make(-1).flip
      yield assertTrue(
        errorZero.isInstanceOf[IllegalArgumentException],
        errorZero.getMessage == "Invalid group ID: 0. Must be positive.",
        errorNegative.isInstanceOf[IllegalArgumentException],
        errorNegative.getMessage == "Invalid group ID: -1. Must be positive."
      )
    },
    test("support ordering and cats order") {
      for
        grp1 <- Group.make(1)
        grp2 <- Group.make(2)
      yield assertTrue(
        Ordering[Group].compare(grp1, grp2) < 0,
        Ordering[Group].compare(grp2, grp1) > 0,
        Ordering[Group].compare(grp1, grp1) == 0,
        Order[Group].compare(grp1, grp2) < 0,
        Order[Group].eqv(grp1, grp1)
      )
    }
  )
