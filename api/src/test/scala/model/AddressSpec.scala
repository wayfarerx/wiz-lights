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
 * Specifications for the Address model.
 */
object AddressSpec extends ZIOSpecDefault:

  override def spec = suite("AddressSpec")(
    test("successfully create valid MAC addresses") {
      for
        addr1 <- Address.make("0123456789ab")
        addr2 <- Address.make("A1B2C3D4E5F6")
        addr3 <- Address.make("  0123456789ab  ")
      yield assertTrue(
        addr1 == "0123456789ab",
        addr2 == "a1b2c3d4e5f6",
        addr3 == "0123456789ab"
      )
    },
    test("fail on empty or blank MAC address") {
      for
        err1 <- Address.make("").flip
        err2 <- Address.make("   ").flip
      yield assertTrue(
        err1.isInstanceOf[IllegalArgumentException],
        err1.getMessage == "Empty or blank MAC address.",
        err2.isInstanceOf[IllegalArgumentException],
        err2.getMessage == "Empty or blank MAC address."
      )
    },
    test("fail on malformed MAC address") {
      for
        errInvalidChar <- Address.make("0123456789ag").flip
        errTooShort <- Address.make("0123456789a").flip
        errTooLong <- Address.make("0123456789abcdef").flip
        errWithColons <- Address.make("01:23:45:67:89:ab").flip
      yield assertTrue(
        errInvalidChar.isInstanceOf[IllegalArgumentException],
        errTooShort.isInstanceOf[IllegalArgumentException],
        errTooLong.isInstanceOf[IllegalArgumentException],
        errWithColons.isInstanceOf[IllegalArgumentException]
      )
    },
    test("support ordering and cats order") {
      for
        addr1 <- Address.make("000000000001")
        addr2 <- Address.make("000000000002")
      yield assertTrue(
        Ordering[Address].compare(addr1, addr2) < 0,
        Ordering[Address].compare(addr2, addr1) > 0,
        Ordering[Address].compare(addr1, addr1) == 0,
        Order[Address].compare(addr1, addr2) < 0,
        Order[Address].eqv(addr1, addr1)
      )
    }
  )
