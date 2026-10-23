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
package service

import zio.{Scope, ZIO}
import zio.test.*

import model.*

/**
 * Specifications for the Update service model.
 */
object UpdateSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("UpdateSpec")(
    test("create and pattern match all Update variations") {
      for
        bright <- Brightness.make(50)
        temp <- Temperature.make(3000)
        col <- Color.make(255, 0, 0)
        spd <- Speed.make(100)
        staticScene = Scene.Static.Cozy
        dynamicScene = Scene.Dynamic.Sunset
        status = Status.White(temp, bright)
        updates: List[Update] = List(
          Update.Enabled(true),
          Update.Brightness(bright),
          Update.Temperature(temp),
          Update.Color(col),
          Update.Static(staticScene),
          Update.Dynamic(dynamicScene, spd),
          Update.Speed(spd),
          Update.Status(status)
        )
        matched = updates.map {
          case Update.Enabled(e) => s"enabled:$e"
          case Update.Brightness(b) => s"brightness:${b: Int}"
          case Update.Temperature(t) => s"temperature:${t: Int}"
          case Update.Color(c) => s"color:${c.red}"
          case Update.Static(s) => s"static:${s.name}"
          case Update.Dynamic(s, sp) => s"dynamic:${s.name}:${sp: Int}"
          case Update.Speed(sp) => s"speed:${sp: Int}"
          case Update.Status(st) => s"status:$st"
        }
      yield assertTrue(
        matched == List(
          "enabled:true",
          "brightness:50",
          "temperature:3000",
          "color:255",
          "static:Cozy",
          "dynamic:Sunset:100",
          "speed:100",
          s"status:$status"
        )
      )
    }
  )
