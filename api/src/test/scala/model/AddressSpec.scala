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
 * Specifications for the Address model.
 */
object AddressSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("AddressSpec")(
    test("successfully create valid MAC addresses") {
      for
        address1 <- Address.make("0123456789ab")
        address2 <- Address.make("A1B2C3D4E5F6")
        address3 <- Address.make("  0123456789ab  ")
      yield assertTrue(
        (address1: String) == "0123456789ab",
        (address2: String) == "a1b2c3d4e5f6",
        (address3: String) == "0123456789ab"
      )
    },
    test("fail on empty or blank MAC address") {
      for
        error1 <- Address.make("").flip
        error2 <- Address.make("   ").flip
      yield assertTrue(
        error1.isInstanceOf[IllegalArgumentException],
        error1.getMessage == "Empty or blank MAC address.",
        error2.isInstanceOf[IllegalArgumentException],
        error2.getMessage == "Empty or blank MAC address."
      )
    },
    test("fail on malformed MAC address") {
      for
        errorInvalidChar <- Address.make("0123456789ag").flip
        errorTooShort <- Address.make("0123456789a").flip
        errorTooLong <- Address.make("0123456789abcdef").flip
        errorWithColons <- Address.make("01:23:45:67:89:ab").flip
      yield assertTrue(
        errorInvalidChar.isInstanceOf[IllegalArgumentException],
        errorTooShort.isInstanceOf[IllegalArgumentException],
        errorTooLong.isInstanceOf[IllegalArgumentException],
        errorWithColons.isInstanceOf[IllegalArgumentException]
      )
    },
    test("support ordering and cats order") {
      for
        address1 <- Address.make("000000000001")
        address2 <- Address.make("000000000002")
      yield assertTrue(
        Ordering[Address].compare(address1, address2) < 0,
        Ordering[Address].compare(address2, address1) > 0,
        Ordering[Address].compare(address1, address1) == 0,
        Order[Address].compare(address1, address2) < 0,
        Order[Address].eqv(address1, address1)
      )
    }
  )
