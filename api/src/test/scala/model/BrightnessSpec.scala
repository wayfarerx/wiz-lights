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
 * Specifications for the Brightness model.
 */
object BrightnessSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("BrightnessSpec")(
    test("predefined brightness constants") {
      assertTrue(
        Brightness.Dimmest.value == 10,
        Brightness.Brightest.value == 100,
        Brightness.Medium.value == 55
      )
    },
    test("successfully create valid brightness values") {
      for
        dimmest <- Brightness.make(10)
        medium <- Brightness.make(55)
        brightest <- Brightness.make(100)
        custom <- Brightness.make(75)
      yield assertTrue(
        dimmest == Brightness.Dimmest,
        medium == Brightness.Medium,
        brightest == Brightness.Brightest,
        custom.value == 75
      )
    },
    test("fail when brightness is out of range") {
      for
        tooLow <- Brightness.make(9).flip
        negative <- Brightness.make(-1).flip
        tooHigh <- Brightness.make(101).flip
      yield assertTrue(
        tooLow.isInstanceOf[IllegalArgumentException],
        tooLow.getMessage == "Invalid brightness value: 9. Must be between 10 and 100.",
        negative.isInstanceOf[IllegalArgumentException],
        tooHigh.isInstanceOf[IllegalArgumentException],
        tooHigh.getMessage == "Invalid brightness value: 101. Must be between 10 and 100."
      )
    }
  )
