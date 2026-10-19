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
 * Specifications for the Scene model.
 */
object SceneSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SceneSpec")(
    test("all scenes have expected IDs, names, and static/dynamic classification") {
      assertTrue(
        Scene.Ocean.id == 1 && Scene.Ocean.name == "Ocean" && Scene.Ocean.isDynamic,
        Scene.Romance.id == 2 && Scene.Romance.name == "Romance" && Scene.Romance.isDynamic,
        Scene.Sunset.id == 3 && Scene.Sunset.name == "Sunset" && Scene.Sunset.isDynamic,
        Scene.Party.id == 4 && Scene.Party.name == "Party" && Scene.Party.isDynamic,
        Scene.Fireplace.id == 5 && Scene.Fireplace.name == "Fireplace" && Scene.Fireplace.isDynamic,
        Scene.Cozy.id == 6 && Scene.Cozy.name == "Cozy" && Scene.Cozy.isStatic,
        Scene.Forest.id == 7 && Scene.Forest.name == "Forest" && Scene.Forest.isDynamic,
        Scene.PastelColors.id == 8 && Scene.PastelColors.name == "Pastel Colors" && Scene.PastelColors.isDynamic,
        Scene.WakeUp.id == 9 && Scene.WakeUp.name == "Wake-Up" && Scene.WakeUp.isDynamic,
        Scene.Bedtime.id == 10 && Scene.Bedtime.name == "Bedtime" && Scene.Bedtime.isDynamic,
        Scene.WarmWhite.id == 11 && Scene.WarmWhite.name == "Warm White" && Scene.WarmWhite.isStatic,
        Scene.Daylight.id == 12 && Scene.Daylight.name == "Daylight" && Scene.Daylight.isStatic,
        Scene.CoolWhite.id == 13 && Scene.CoolWhite.name == "Cool White" && Scene.CoolWhite.isStatic,
        Scene.NightLight.id == 14 && Scene.NightLight.name == "Night Light" && Scene.NightLight.isStatic,
        Scene.Focus.id == 15 && Scene.Focus.name == "Focus" && Scene.Focus.isStatic,
        Scene.Relax.id == 16 && Scene.Relax.name == "Relax" && Scene.Relax.isStatic,
        Scene.TrueColors.id == 17 && Scene.TrueColors.name == "True Colors" && Scene.TrueColors.isStatic,
        Scene.TVTime.id == 18 && Scene.TVTime.name == "TV Time" && Scene.TVTime.isStatic,
        Scene.PlantGrowth.id == 19 && Scene.PlantGrowth.name == "Plant Growth" && Scene.PlantGrowth.isStatic,
        Scene.Spring.id == 20 && Scene.Spring.name == "Spring" && Scene.Spring.isDynamic,
        Scene.Summer.id == 21 && Scene.Summer.name == "Summer" && Scene.Summer.isDynamic,
        Scene.Fall.id == 22 && Scene.Fall.name == "Fall" && Scene.Fall.isDynamic,
        Scene.DeepDive.id == 23 && Scene.DeepDive.name == "Deep Dive" && Scene.DeepDive.isDynamic,
        Scene.Jungle.id == 24 && Scene.Jungle.name == "Jungle" && Scene.Jungle.isDynamic,
        Scene.Mojito.id == 25 && Scene.Mojito.name == "Mojito" && Scene.Mojito.isDynamic,
        Scene.Club.id == 26 && Scene.Club.name == "Club" && Scene.Club.isDynamic,
        Scene.Christmas.id == 27 && Scene.Christmas.name == "Christmas" && Scene.Christmas.isDynamic,
        Scene.Halloween.id == 28 && Scene.Halloween.name == "Halloween" && Scene.Halloween.isDynamic,
        Scene.Candlelight.id == 29 && Scene.Candlelight.name == "Candlelight" && Scene.Candlelight.isDynamic,
        Scene.GoldenWhite.id == 30 && Scene.GoldenWhite.name == "Golden White" && Scene.GoldenWhite.isDynamic,
        Scene.Pulse.id == 31 && Scene.Pulse.name == "Pulse" && Scene.Pulse.isDynamic,
        Scene.Steampunk.id == 32 && Scene.Steampunk.name == "Steampunk" && Scene.Steampunk.isDynamic,
        Scene.Diwali.id == 33 && Scene.Diwali.name == "Diwali" && Scene.Diwali.isDynamic,
        Scene.White.id == 34 && Scene.White.name == "White" && Scene.White.isStatic,
        Scene.Alarm.id == 35 && Scene.Alarm.name == "Alarm" && Scene.Alarm.isDynamic,
        Scene.SnowySky.id == 36 && Scene.SnowySky.name == "Snowy Sky" && Scene.SnowySky.isDynamic,
        Scene.User1.id == 256 && Scene.User1.name == "User 1" && Scene.User1.isDynamic,
        Scene.User2.id == 257 && Scene.User2.name == "User 2" && Scene.User2.isDynamic,
        Scene.User3.id == 258 && Scene.User3.name == "User 3" && Scene.User3.isDynamic,
        Scene.User4.id == 259 && Scene.User4.name == "User 4" && Scene.User4.isDynamic,
        Scene.User5.id == 260 && Scene.User5.name == "User 5" && Scene.User5.isDynamic,
        Scene.User6.id == 261 && Scene.User6.name == "User 6" && Scene.User6.isDynamic,
        Scene.User7.id == 262 && Scene.User7.name == "User 7" && Scene.User7.isDynamic,
        Scene.User8.id == 263 && Scene.User8.name == "User 8" && Scene.User8.isDynamic,
        Scene.User9.id == 264 && Scene.User9.name == "User 9" && Scene.User9.isDynamic,
        Scene.User10.id == 265 && Scene.User10.name == "User 10" && Scene.User10.isDynamic,
        Scene.Rhythm.id == 1000 && Scene.Rhythm.name == "Rhythm" && Scene.Rhythm.isStatic,
        Scene.values.length == 47,
        Scene.Static.values.length == 12,
        Scene.Dynamic.values.length == 35
      )
    },
    test("Scene.make by ID") {
      for
        ocean <- Scene.make(1)
        rhythm <- Scene.make(1000)
        user1 <- Scene.make(256)
        invalidError <- Scene.make(9999).flip
      yield assertTrue(
        ocean == Scene.Ocean,
        rhythm == Scene.Rhythm,
        user1 == Scene.User1,
        invalidError.isInstanceOf[IllegalArgumentException],
        invalidError.getMessage == "Invalid scene ID: 9999."
      )
    },
    test("Scene.make by name") {
      for
        oceanExact <- Scene.make("Ocean")
        oceanLower <- Scene.make("ocean")
        oceanTrimmed <- Scene.make("  ocean  ")
        user1 <- Scene.make("User 1")
        rhythm <- Scene.make("rhythm")
        emptyError <- Scene.make("").flip
        unknownError <- Scene.make("unknown").flip
      yield assertTrue(
        oceanExact == Scene.Ocean,
        oceanLower == Scene.Ocean,
        oceanTrimmed == Scene.Ocean,
        user1 == Scene.User1,
        rhythm == Scene.Rhythm,
        emptyError.isInstanceOf[IllegalArgumentException],
        emptyError.getMessage == "Invalid scene name: .",
        unknownError.isInstanceOf[IllegalArgumentException],
        unknownError.getMessage == "Invalid scene name: unknown."
      )
    },
    test("Scene.valueBy ID and name") {
      assertTrue(
        Scene.valueBy(1).contains(Scene.Ocean),
        Scene.valueBy(256).contains(Scene.User1),
        Scene.valueBy(1000).contains(Scene.Rhythm),
        Scene.valueBy(9999).isEmpty,
        Scene.valueBy("Ocean").contains(Scene.Ocean),
        Scene.valueBy("  ocean  ").contains(Scene.Ocean),
        Scene.valueBy("User 1").contains(Scene.User1),
        Scene.valueBy("Rhythm").contains(Scene.Rhythm),
        Scene.valueBy("unknown").isEmpty
      )
    }
  )
