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
 * Specifications for the Module model.
 */
object ModuleSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ModuleSpec")(
    test("successfully create valid module names") {
      for
        mod1 <- Module.make("ESP01_SHRGB_03")
        mod2 <- Module.make("  ESP01_SHRGB_03  ")
      yield assertTrue(
        (mod1: String) == "ESP01_SHRGB_03",
        (mod2: String) == "ESP01_SHRGB_03"
      )
    },
    test("fail when module name is empty or blank") {
      for
        errorEmpty <- Module.make("").flip
        errorBlank <- Module.make("   ").flip
      yield assertTrue(
        errorEmpty.isInstanceOf[IllegalArgumentException],
        errorEmpty.getMessage == "Empty or blank module name.",
        errorBlank.isInstanceOf[IllegalArgumentException],
        errorBlank.getMessage == "Empty or blank module name."
      )
    },
    test("support ordering and cats order") {
      for
        mod1 <- Module.make("ModuleA")
        mod2 <- Module.make("ModuleB")
      yield assertTrue(
        Ordering[Module].compare(mod1, mod2) < 0,
        Ordering[Module].compare(mod2, mod1) > 0,
        Ordering[Module].compare(mod1, mod1) == 0,
        Order[Module].compare(mod1, mod2) < 0,
        Order[Module].eqv(mod1, mod1)
      )
    }
  )
