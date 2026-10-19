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
 * Specifications for the Color model.
 */
object ColorSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ColorSpec")(
    test("predefined color constants") {
      assertTrue(
        Color.Black.red == 0 && Color.Black.green == 0 && Color.Black.blue == 0 && Color.Black.cool == 0 && Color.Black.warm == 0,
        Color.Silver.red == 192 && Color.Silver.green == 192 && Color.Silver.blue == 192,
        Color.Gray.red == 128 && Color.Gray.green == 128 && Color.Gray.blue == 128,
        Color.White.red == 255 && Color.White.green == 255 && Color.White.blue == 255,
        Color.Maroon.red == 128 && Color.Maroon.green == 0 && Color.Maroon.blue == 0,
        Color.Red.red == 255 && Color.Red.green == 0 && Color.Red.blue == 0,
        Color.Purple.red == 128 && Color.Purple.green == 0 && Color.Purple.blue == 128,
        Color.Fuchsia.red == 255 && Color.Fuchsia.green == 0 && Color.Fuchsia.blue == 255,
        Color.Green.red == 0 && Color.Green.green == 128 && Color.Green.blue == 0,
        Color.Lime.red == 0 && Color.Lime.green == 255 && Color.Lime.blue == 0,
        Color.Olive.red == 128 && Color.Olive.green == 128 && Color.Olive.blue == 0,
        Color.Yellow.red == 255 && Color.Yellow.green == 255 && Color.Yellow.blue == 0,
        Color.Navy.red == 0 && Color.Navy.green == 0 && Color.Navy.blue == 128,
        Color.Blue.red == 0 && Color.Blue.green == 0 && Color.Blue.blue == 255,
        Color.Teal.red == 0 && Color.Teal.green == 128 && Color.Teal.blue == 128,
        Color.Aqua.red == 0 && Color.Aqua.green == 255 && Color.Aqua.blue == 255
      )
    },
    test("successfully create valid colors") {
      for
        black <- Color.make(0, 0, 0)
        white <- Color.make(255, 255, 255)
        customRGB <- Color.make(10, 20, 30)
        custom5Channel <- Color.make(10, 20, 30, 40, 50)
      yield assertTrue(
        black == Color.Black,
        white == Color.White,
        customRGB.red == 10,
        customRGB.green == 20,
        customRGB.blue == 30,
        customRGB.cool == 0,
        customRGB.warm == 0,
        custom5Channel.red == 10,
        custom5Channel.green == 20,
        custom5Channel.blue == 30,
        custom5Channel.cool == 40,
        custom5Channel.warm == 50
      )
    },
    test("fail when color components are out of range") {
      for
        badRedLow <- Color.make(-1, 0, 0).flip
        badRedHigh <- Color.make(256, 0, 0).flip
        badGreenLow <- Color.make(0, -1, 0).flip
        badGreenHigh <- Color.make(0, 256, 0).flip
        badBlueLow <- Color.make(0, 0, -1).flip
        badBlueHigh <- Color.make(0, 0, 256).flip
        badCoolLow <- Color.make(0, 0, 0, -1, 0).flip
        badCoolHigh <- Color.make(0, 0, 0, 256, 0).flip
        badWarmLow <- Color.make(0, 0, 0, 0, -1).flip
        badWarmHigh <- Color.make(0, 0, 0, 0, 256).flip
      yield assertTrue(
        badRedLow.isInstanceOf[IllegalArgumentException],
        badRedLow.getMessage == "Invalid red color component: -1. Must be between 0 and 255.",
        badRedHigh.isInstanceOf[IllegalArgumentException],
        badGreenLow.isInstanceOf[IllegalArgumentException],
        badGreenHigh.isInstanceOf[IllegalArgumentException],
        badBlueLow.isInstanceOf[IllegalArgumentException],
        badBlueHigh.isInstanceOf[IllegalArgumentException],
        badCoolLow.isInstanceOf[IllegalArgumentException],
        badCoolHigh.isInstanceOf[IllegalArgumentException],
        badWarmLow.isInstanceOf[IllegalArgumentException],
        badWarmHigh.isInstanceOf[IllegalArgumentException]
      )
    }
  )
