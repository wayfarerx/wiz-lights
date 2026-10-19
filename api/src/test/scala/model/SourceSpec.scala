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
 * Specifications for the Source model.
 */
object SourceSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SourceSpec")(
    test("predefined source constants") {
      assertTrue(
        (Source.Heartbeat: String) == "hb",
        (Source.Udp: String) == "udp",
        (Source.App: String) == "wfa",
        (Source.Remote: String) == "wzm",
        (Source.Sensor: String) == "pir"
      )
    },
    test("successfully create valid sources") {
      for
        hb <- Source.make("hb")
        custom <- Source.make("  custom  ")
      yield assertTrue(
        hb == Source.Heartbeat,
        (custom: String) == "custom"
      )
    },
    test("fail when source is empty or blank") {
      for
        errorEmpty <- Source.make("").flip
        errorBlank <- Source.make("   ").flip
      yield assertTrue(
        errorEmpty.isInstanceOf[IllegalArgumentException],
        errorEmpty.getMessage == "Empty or blank source.",
        errorBlank.isInstanceOf[IllegalArgumentException],
        errorBlank.getMessage == "Empty or blank source."
      )
    },
    test("support ordering and cats order") {
      for
        src1 <- Source.make("a")
        src2 <- Source.make("b")
      yield assertTrue(
        Ordering[Source].compare(src1, src2) < 0,
        Ordering[Source].compare(src2, src1) > 0,
        Ordering[Source].compare(src1, src1) == 0,
        Order[Source].compare(src1, src2) < 0,
        Order[Source].eqv(src1, src1)
      )
    }
  )
