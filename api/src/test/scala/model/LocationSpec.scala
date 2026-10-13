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
 * Specifications for the Location model.
 */
object LocationSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LocationSpec")(
    test("successfully create valid locations and trim name") {
      for
        address <- Address.make("0123456789ab")
        location1 <- Location.make("Living Room", address)
        location2 <- Location.make("  Living Room  ", address)
      yield assertTrue(
        location1.name == "Living Room",
        location1.macAddress == address,
        location2.name == "Living Room",
        location1 == location2
      )
    },
    test("fail when name is empty or blank") {
      for
        address <- Address.make("0123456789ab")
        error1 <- Location.make("", address).flip
        error2 <- Location.make("   ", address).flip
      yield assertTrue(
        error1.isInstanceOf[IllegalArgumentException],
        error1.getMessage == "Empty or blank location name.",
        error2.isInstanceOf[IllegalArgumentException],
        error2.getMessage == "Empty or blank location name."
      )
    },
    test("support ordering and cats order") {
      for
        address1 <- Address.make("000000000001")
        address2 <- Address.make("000000000002")
        locationA1 <- Location.make("Alpha", address1)
        locationA2 <- Location.make("Alpha", address2)
        locationB1 <- Location.make("Beta", address1)
      yield assertTrue(
        Ordering[Location].compare(locationA1, locationB1) < 0,
        Ordering[Location].compare(locationB1, locationA1) > 0,
        Ordering[Location].compare(locationA1, locationA2) < 0,
        Ordering[Location].compare(locationA2, locationA1) > 0,
        Ordering[Location].compare(locationA1, locationA1) == 0,
        Order[Location].compare(locationA1, locationB1) < 0,
        Order[Location].eqv(locationA1, locationA1)
      )
    }
  )
