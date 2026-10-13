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

import zio.Scope
import zio.test.*

/**
 * Specifications for the Temperature model.
 */
object TemperatureSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TemperatureSpec")(
    test("predefined temperature constants") {
      assertTrue(
        Temperature.Warmest.value == 2200,
        Temperature.Coolest.value == 6500,
        Temperature.Medium.value == 4350
      )
    },
    test("successfully create valid temperature values") {
      for
        warmest <- Temperature.make(2200)
        medium <- Temperature.make(4350)
        coolest <- Temperature.make(6500)
        custom <- Temperature.make(3000)
      yield assertTrue(
        warmest == Temperature.Warmest,
        medium == Temperature.Medium,
        coolest == Temperature.Coolest,
        custom.value == 3000
      )
    },
    test("fail when temperature is out of range") {
      for
        tooWarm <- Temperature.make(2199).flip
        tooCool <- Temperature.make(6501).flip
      yield assertTrue(
        tooWarm.isInstanceOf[IllegalArgumentException],
        tooWarm.getMessage == "Invalid temperature value: 2199. Must be between 2200 and 6500.",
        tooCool.isInstanceOf[IllegalArgumentException],
        tooCool.getMessage == "Invalid temperature value: 6501. Must be between 2200 and 6500."
      )
    }
  )
