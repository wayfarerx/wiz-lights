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
 * Specifications for the Light model.
 */
object LightSpec extends ZIOSpecDefault:

  override def spec = suite("LightSpec")(
    test("create light and access properties") {
      for
        addr <- Address.make("0123456789ab")
        loc <- Location.make("Living Room", addr)
        brightness <- Brightness.make(100)
        temp <- Temperature.make(4000)
        status = Status.White(temp, brightness)
        lightOn = Light(loc, Some(status))
        lightOff = Light(loc, None)
      yield assertTrue(
        lightOn.location == loc,
        lightOn.status.contains(status),
        lightOn.name == "Living Room",
        lightOff.location == loc,
        lightOff.status.isEmpty,
        lightOff.name == "Living Room"
      )
    },
    test("support ordering and cats order based on location") {
      for
        addr1 <- Address.make("000000000001")
        addr2 <- Address.make("000000000002")
        loc1 <- Location.make("Kitchen", addr1)
        loc2 <- Location.make("Living Room", addr2)
        light1 = Light(loc1, None)
        light2 = Light(loc2, Some(Status.Disabled))
        light1WithStatus = Light(loc1, Some(Status.Disabled))
      yield assertTrue(
        Ordering[Light].compare(light1, light2) < 0,
        Ordering[Light].compare(light2, light1) > 0,
        Ordering[Light].compare(light1, light1WithStatus) == 0,
        Order[Light].compare(light1, light2) < 0,
        Order[Light].eqv(light1, light1WithStatus)
      )
    }
  )
