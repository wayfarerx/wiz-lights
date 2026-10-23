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

import io.circe.{Decoder, Encoder, Json}

import zio.{Scope, ZIO}
import zio.test.*

/**
 * Specifications for the Scene model.
 */
object SceneSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SceneSpec")(
    test("distinguish between static and dynamic scenes") {
      val staticScene = Scene.Static.Cozy
      val dynamicScene = Scene.Dynamic.Ocean
      assertTrue(
        staticScene.isStatic,
        !staticScene.isDynamic,
        !dynamicScene.isStatic,
        dynamicScene.isDynamic,
        staticScene.id == 6,
        staticScene.name == "Cozy",
        dynamicScene.id == 1,
        dynamicScene.name == "Ocean"
      )
    },
    test("lookup scenes by ID and name on Scene companion") {
      for
        sceneById <- Scene.make(11)
        sceneByName <- Scene.make("Warm White")
        sceneByNameCaseInsensitive <- Scene.make("  warm white  ")
        errId <- Scene.make(9999).flip
        errName <- Scene.make("Nonexistent Scene").flip
      yield assertTrue(
        sceneById == Scene.Static.WarmWhite,
        sceneByName == Scene.Static.WarmWhite,
        sceneByNameCaseInsensitive == Scene.Static.WarmWhite,
        errId.getMessage == "Invalid scene ID: 9999.",
        errName.getMessage == "Invalid scene name: Nonexistent Scene."
      )
    },
    test("lookup static scenes on Scene.Static companion") {
      for
        staticById <- Scene.Static.make(14)
        staticByName <- Scene.Static.make("Night Light")
        errDynamicId <- Scene.Static.make(1).flip
        errDynamicName <- Scene.Static.make("Ocean").flip
      yield assertTrue(
        staticById == Scene.Static.NightLight,
        staticByName == Scene.Static.NightLight,
        errDynamicId.getMessage == "Invalid static scene ID: 1.",
        errDynamicName.getMessage == "Invalid static scene name: Ocean."
      )
    },
    test("lookup dynamic scenes on Scene.Dynamic companion") {
      for
        dynById <- Scene.Dynamic.make(3)
        dynByName <- Scene.Dynamic.make("Sunset")
        errStaticId <- Scene.Dynamic.make(6).flip
        errStaticName <- Scene.Dynamic.make("Cozy").flip
      yield assertTrue(
        dynById == Scene.Dynamic.Sunset,
        dynByName == Scene.Dynamic.Sunset,
        errStaticId.getMessage == "Invalid dynamic scene ID: 6.",
        errStaticName.getMessage == "Invalid dynamic scene name: Cozy."
      )
    },
    test("contain all scenes in Scene.values") {
      assertTrue(
        Scene.values.size == Scene.Static.values.length + Scene.Dynamic.values.length,
        Scene.values.contains(Scene.Static.Cozy),
        Scene.values.contains(Scene.Dynamic.Party)
      )
    },
    test("encode and decode Scene as JSON by ID and by name") {
      val staticScene: Scene = Scene.Static.Daylight
      val dynamicScene: Scene = Scene.Dynamic.Fireplace

      val encStatic = Encoder[Scene].apply(staticScene)
      val encDynamic = Encoder[Scene].apply(dynamicScene)

      val decStaticFromId = Decoder[Scene].decodeJson(Json.fromInt(12))
      val decStaticFromName = Decoder[Scene].decodeJson(Json.fromString("Daylight"))
      val decDynamicFromId = Decoder[Scene].decodeJson(Json.fromInt(5))
      val decDynamicFromName = Decoder[Scene].decodeJson(Json.fromString("Fireplace"))
      val decInvalidId = Decoder[Scene].decodeJson(Json.fromInt(-1))
      val decInvalidName = Decoder[Scene].decodeJson(Json.fromString("Unknown"))
      val decInvalidType = Decoder[Scene].decodeJson(Json.arr())

      assertTrue(
        encStatic == Json.fromInt(12),
        encDynamic == Json.fromInt(5),
        decStaticFromId == Right(staticScene),
        decStaticFromName == Right(staticScene),
        decDynamicFromId == Right(dynamicScene),
        decDynamicFromName == Right(dynamicScene),
        decInvalidId.isLeft,
        decInvalidName.isLeft,
        decInvalidType.isLeft
      )
    },
    test("encode and decode Scene.Static and Scene.Dynamic as JSON") {
      val s = Scene.Static.Focus
      val d = Scene.Dynamic.Forest

      val encS = Encoder[Scene.Static].apply(s)
      val decSFromId = Decoder[Scene.Static].decodeJson(Json.fromInt(15))
      val decSFromName = Decoder[Scene.Static].decodeJson(Json.fromString("Focus"))
      val decSInvalid = Decoder[Scene.Static].decodeJson(Json.fromInt(1))

      val encD = Encoder[Scene.Dynamic].apply(d)
      val decDFromId = Decoder[Scene.Dynamic].decodeJson(Json.fromInt(7))
      val decDFromName = Decoder[Scene.Dynamic].decodeJson(Json.fromString("Forest"))
      val decDInvalid = Decoder[Scene.Dynamic].decodeJson(Json.fromInt(6))

      assertTrue(
        encS == Json.fromInt(15),
        decSFromId == Right(s),
        decSFromName == Right(s),
        decSInvalid.isLeft,
        encD == Json.fromInt(7),
        decDFromId == Right(d),
        decDFromName == Right(d),
        decDInvalid.isLeft
      )
    }
  )
