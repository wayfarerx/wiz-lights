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
 * Specifications for the Light model.
 */
object LightSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightSpec")(
    test("create light and access properties") {
      for
        address <- Address.make("0123456789ab")
        location <- Location.make("Living Room", address)
        brightness <- Brightness.make(100)
        temperature <- Temperature.make(4000)
        status = Status.White(temperature, brightness)
        lightOn = Light(location, Some(status))
        lightOff = Light(location, None)
      yield assertTrue(
        lightOn.location == location,
        lightOn.status.contains(status),
        lightOn.name == "Living Room",
        lightOff.location == location,
        lightOff.status.isEmpty,
        lightOff.name == "Living Room"
      )
    },
    test("support ordering and cats order based on location") {
      for
        address1 <- Address.make("000000000001")
        address2 <- Address.make("000000000002")
        location1 <- Location.make("Kitchen", address1)
        location2 <- Location.make("Living Room", address2)
        light1 = Light(location1, None)
        light2 = Light(location2, Some(Status.Disabled))
        light1WithStatus = Light(location1, Some(Status.Disabled))
      yield assertTrue(
        Ordering[Light].compare(light1, light2) < 0,
        Ordering[Light].compare(light2, light1) > 0,
        Ordering[Light].compare(light1, light1WithStatus) == 0,
        Order[Light].compare(light1, light2) < 0,
        Order[Light].eqv(light1, light1WithStatus)
      )
    }
  )
