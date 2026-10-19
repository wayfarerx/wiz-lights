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
 * Specifications for the Device model.
 */
object DeviceSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("DeviceSpec")(
    test("successfully create valid device type IDs") {
      for
        zero <- Device.make(0)
        custom <- Device.make(1234)
      yield assertTrue(
        (zero: Int) == 0,
        (custom: Int) == 1234
      )
    },
    test("fail when device type ID is negative") {
      for
        error <- Device.make(-1).flip
      yield assertTrue(
        error.isInstanceOf[IllegalArgumentException],
        error.getMessage == "Invalid device type ID: -1. Cannot be negative."
      )
    },
    test("support ordering and cats order") {
      for
        dev1 <- Device.make(1)
        dev2 <- Device.make(2)
      yield assertTrue(
        Ordering[Device].compare(dev1, dev2) < 0,
        Ordering[Device].compare(dev2, dev1) > 0,
        Ordering[Device].compare(dev1, dev1) == 0,
        Order[Device].compare(dev1, dev2) < 0,
        Order[Device].eqv(dev1, dev1)
      )
    }
  )
