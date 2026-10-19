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
 * Specifications for the Signal model.
 */
object SignalSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SignalSpec")(
    test("predefined signal constants") {
      assertTrue(
        (Signal.Weakest: Int) == -95,
        (Signal.Strongest: Int) == -20,
        (Signal.Medium: Int) == (-20 - -95) / 2 + -95
      )
    },
    test("successfully create valid signal values") {
      for
        weakest <- Signal.make(-95)
        medium <- Signal.make((-20 - -95) / 2 + -95)
        strongest <- Signal.make(-20)
        custom <- Signal.make(-50)
      yield assertTrue(
        weakest == Signal.Weakest,
        medium == Signal.Medium,
        strongest == Signal.Strongest,
        (custom: Int) == -50
      )
    },
    test("fail when signal is out of range") {
      for
        tooWeak <- Signal.make(-96).flip
        tooStrong <- Signal.make(-19).flip
      yield assertTrue(
        tooWeak.isInstanceOf[IllegalArgumentException],
        tooWeak.getMessage == "Invalid signal value: -96. Must be between -95 and -20.",
        tooStrong.isInstanceOf[IllegalArgumentException],
        tooStrong.getMessage == "Invalid signal value: -19. Must be between -95 and -20."
      )
    }
  )
