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
import zio.test.*

/**
 * Specifications for the Location model.
 */
object LocationSpec extends ZIOSpecDefault:

  override def spec = suite("LocationSpec")(
    test("successfully create valid locations and trim name") {
      for
        addr <- Address.make("0123456789ab")
        loc1 <- Location.make("Living Room", addr)
        loc2 <- Location.make("  Living Room  ", addr)
      yield assertTrue(
        loc1.name == "Living Room",
        loc1.macAddress == addr,
        loc2.name == "Living Room",
        loc1 == loc2
      )
    },
    test("fail when name is empty or blank") {
      for
        addr <- Address.make("0123456789ab")
        err1 <- Location.make("", addr).flip
        err2 <- Location.make("   ", addr).flip
      yield assertTrue(
        err1.isInstanceOf[IllegalArgumentException],
        err1.getMessage == "Empty or blank location name.",
        err2.isInstanceOf[IllegalArgumentException],
        err2.getMessage == "Empty or blank location name."
      )
    },
    test("support ordering and cats order") {
      for
        addr1 <- Address.make("000000000001")
        addr2 <- Address.make("000000000002")
        locA1 <- Location.make("Alpha", addr1)
        locA2 <- Location.make("Alpha", addr2)
        locB1 <- Location.make("Beta", addr1)
      yield assertTrue(
        Ordering[Location].compare(locA1, locB1) < 0,
        Ordering[Location].compare(locB1, locA1) > 0,
        Ordering[Location].compare(locA1, locA2) < 0,
        Ordering[Location].compare(locA2, locA1) > 0,
        Ordering[Location].compare(locA1, locA1) == 0,
        Order[Location].compare(locA1, locB1) < 0,
        Order[Location].eqv(locA1, locA1)
      )
    }
  )
