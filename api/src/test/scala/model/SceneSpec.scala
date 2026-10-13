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

import zio.test.*

/**
 * Specifications for the Scene model.
 */
object SceneSpec extends ZIOSpecDefault:

  override def spec = suite("SceneSpec")(
    test("all scenes have expected IDs and names") {
      assertTrue(
        Scene.Ocean.id == 1 && Scene.Ocean.name == "Ocean",
        Scene.Romance.id == 2 && Scene.Romance.name == "Romance",
        Scene.Sunset.id == 3 && Scene.Sunset.name == "Sunset",
        Scene.Party.id == 4 && Scene.Party.name == "Party",
        Scene.Fireplace.id == 5 && Scene.Fireplace.name == "Fireplace",
        Scene.Cozy.id == 6 && Scene.Cozy.name == "Cozy",
        Scene.Forest.id == 7 && Scene.Forest.name == "Forest",
        Scene.PastelColors.id == 8 && Scene.PastelColors.name == "Pastel Colors",
        Scene.WakeUp.id == 9 && Scene.WakeUp.name == "Wake-Up",
        Scene.Bedtime.id == 10 && Scene.Bedtime.name == "Bedtime",
        Scene.WarmWhite.id == 11 && Scene.WarmWhite.name == "Warm White",
        Scene.Daylight.id == 12 && Scene.Daylight.name == "Daylight",
        Scene.CoolWhite.id == 13 && Scene.CoolWhite.name == "Cool White",
        Scene.NightLight.id == 14 && Scene.NightLight.name == "Night Light",
        Scene.Focus.id == 15 && Scene.Focus.name == "Focus",
        Scene.Relax.id == 16 && Scene.Relax.name == "Relax",
        Scene.TrueColors.id == 17 && Scene.TrueColors.name == "True Colors",
        Scene.TVTime.id == 18 && Scene.TVTime.name == "TV Time",
        Scene.PlantGrowth.id == 19 && Scene.PlantGrowth.name == "Plant Growth",
        Scene.Spring.id == 20 && Scene.Spring.name == "Spring",
        Scene.Summer.id == 21 && Scene.Summer.name == "Summer",
        Scene.Fall.id == 22 && Scene.Fall.name == "Fall",
        Scene.DeepDive.id == 23 && Scene.DeepDive.name == "Deep Dive",
        Scene.Jungle.id == 24 && Scene.Jungle.name == "Jungle",
        Scene.Mojito.id == 25 && Scene.Mojito.name == "Mojito",
        Scene.Club.id == 26 && Scene.Club.name == "Club",
        Scene.Christmas.id == 27 && Scene.Christmas.name == "Christmas",
        Scene.Halloween.id == 28 && Scene.Halloween.name == "Halloween",
        Scene.Candlelight.id == 29 && Scene.Candlelight.name == "Candlelight",
        Scene.GoldenWhite.id == 30 && Scene.GoldenWhite.name == "Golden White",
        Scene.Pulse.id == 31 && Scene.Pulse.name == "Pulse",
        Scene.Steampunk.id == 32 && Scene.Steampunk.name == "Steampunk",
        Scene.Diwali.id == 33 && Scene.Diwali.name == "Diwali",
        Scene.White.id == 34 && Scene.White.name == "White",
        Scene.Alarm.id == 35 && Scene.Alarm.name == "Alarm",
        Scene.SnowySky.id == 36 && Scene.SnowySky.name == "Snowy Sky",
        Scene.Rhythm.id == 1000 && Scene.Rhythm.name == "Rhythm",
        Scene.values.length == 37
      )
    },
    test("Scene.make by ID") {
      for
        ocean <- Scene.make(1)
        rhythm <- Scene.make(1000)
        invalidErr <- Scene.make(9999).flip
      yield assertTrue(
        ocean == Scene.Ocean,
        rhythm == Scene.Rhythm,
        invalidErr.isInstanceOf[IllegalArgumentException],
        invalidErr.getMessage == "Invalid scene ID: 9999."
      )
    },
    test("Scene.make by name") {
      for
        oceanExact <- Scene.make("Ocean")
        oceanLower <- Scene.make("ocean")
        oceanTrimmed <- Scene.make("  ocean  ")
        rhythm <- Scene.make("rhythm")
        emptyErr <- Scene.make("").flip
        unknownErr <- Scene.make("unknown").flip
      yield assertTrue(
        oceanExact == Scene.Ocean,
        oceanLower == Scene.Ocean,
        oceanTrimmed == Scene.Ocean,
        rhythm == Scene.Rhythm,
        emptyErr.isInstanceOf[IllegalArgumentException],
        emptyErr.getMessage == "Invalid scene name: .",
        unknownErr.isInstanceOf[IllegalArgumentException],
        unknownErr.getMessage == "Invalid scene name: unknown."
      )
    },
    test("Scene.valueBy ID and name") {
      assertTrue(
        Scene.valueBy(1).contains(Scene.Ocean),
        Scene.valueBy(1000).contains(Scene.Rhythm),
        Scene.valueBy(9999).isEmpty,
        Scene.valueBy("Ocean").contains(Scene.Ocean),
        Scene.valueBy("  ocean  ").contains(Scene.Ocean),
        Scene.valueBy("Rhythm").contains(Scene.Rhythm),
        Scene.valueBy("unknown").isEmpty
      )
    }
  )
