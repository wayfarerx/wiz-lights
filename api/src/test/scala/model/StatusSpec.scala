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
 * Specifications for the Status model.
 */
object StatusSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("StatusSpec")(
    test("support Disabled status") {
      val status: Status = Status.Disabled
      assertTrue(
        status match
          case Status.Disabled => true
          case _ => false
      )
    },
    test("support White status") {
      for
        temperature <- Temperature.make(3000)
        brightness <- Brightness.make(80)
      yield
        val status: Status = Status.White(temperature, brightness)
        assertTrue(
          status match
            case Status.White(t, b) => t == temperature && b == brightness
            case _ => false
        )
    },
    test("support Defined status") {
      for
        brightness <- Brightness.make(90)
      yield
        val status: Status = Status.Defined(Scene.Ocean, brightness)
        assertTrue(
          status match
            case Status.Defined(s, b) => s == Scene.Ocean && b == brightness
            case _ => false
        )
    },
    test("support Custom status") {
      for
        color <- Color.make(255, 100, 50)
        brightness <- Brightness.make(100)
      yield
        val status: Status = Status.Custom(color, brightness)
        assertTrue(
          status match
            case Status.Custom(c, b) => c == color && b == brightness
            case _ => false
        )
    }
  )
