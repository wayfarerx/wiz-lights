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
 * Specifications for the Speed model.
 */
object SpeedSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SpeedSpec")(
    test("predefined speed constants") {
      assertTrue(
        (Speed.Slowest: Int) == 10,
        (Speed.Fastest: Int) == 200,
        (Speed.Medium: Int) == (200 - 10) / 2 + 10
      )
    },
    test("successfully create valid speed values") {
      for
        slowest <- Speed.make(10)
        medium <- Speed.make(105)
        fastest <- Speed.make(200)
        custom <- Speed.make(50)
      yield assertTrue(
        slowest == Speed.Slowest,
        medium == Speed.Medium,
        fastest == Speed.Fastest,
        (custom: Int) == 50
      )
    },
    test("fail when speed is out of range") {
      for
        tooSlow <- Speed.make(9).flip
        tooFast <- Speed.make(201).flip
      yield assertTrue(
        tooSlow.isInstanceOf[IllegalArgumentException],
        tooSlow.getMessage == "Invalid speed value: 9. Must be between 10 and 200.",
        tooFast.isInstanceOf[IllegalArgumentException],
        tooFast.getMessage == "Invalid speed value: 201. Must be between 10 and 200."
      )
    }
  )
