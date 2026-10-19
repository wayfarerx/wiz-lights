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
 * Specifications for the Firmware model.
 */
object FirmwareSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("FirmwareSpec")(
    test("successfully create valid firmware versions") {
      for
        fw1 <- Firmware.make("1.26.0")
        fw2 <- Firmware.make("  1.26.0  ")
      yield assertTrue(
        (fw1: String) == "1.26.0",
        (fw2: String) == "1.26.0"
      )
    },
    test("fail when firmware version is empty or blank") {
      for
        errorEmpty <- Firmware.make("").flip
        errorBlank <- Firmware.make("   ").flip
      yield assertTrue(
        errorEmpty.isInstanceOf[IllegalArgumentException],
        errorEmpty.getMessage == "Empty or blank firmware version.",
        errorBlank.isInstanceOf[IllegalArgumentException],
        errorBlank.getMessage == "Empty or blank firmware version."
      )
    },
    test("support ordering and cats order") {
      for
        fw1 <- Firmware.make("1.0.0")
        fw2 <- Firmware.make("2.0.0")
      yield assertTrue(
        Ordering[Firmware].compare(fw1, fw2) < 0,
        Ordering[Firmware].compare(fw2, fw1) > 0,
        Ordering[Firmware].compare(fw1, fw1) == 0,
        Order[Firmware].compare(fw1, fw2) < 0,
        Order[Firmware].eqv(fw1, fw1)
      )
    }
  )
