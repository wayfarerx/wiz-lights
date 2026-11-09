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

package net.wayfarerx.wizlights.model

import io.circe.{Decoder, Encoder, Json}

import zio.test.*
import zio.{Scope, ZIO}

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
          Update.Scene(staticScene),
          Update.Dynamic(dynamicScene, spd),
          Update.Speed(spd),
          Update.Status(status)
        )
        matched = updates.map {
          case Update.Enabled(e) => s"enabled:$e"
          case Update.Brightness(b) => s"brightness:${b: Int}"
          case Update.Temperature(t) => s"temperature:${t: Int}"
          case Update.Color(c) => s"color:${c.red}"
          case Update.Scene(s) => s"static:${s.name}"
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
    },
    test("encode and decode all Update variations as JSON") {
      for
        bright <- Brightness.make(50)
        temp <- Temperature.make(3000)
        col <- Color.make(255, 0, 0)
        spd <- Speed.make(100)
        staticScene = Scene.Static.Cozy
        dynamicScene = Scene.Dynamic.Sunset
        status = Status.White(temp, bright)
        upEnabled = Update.Enabled(true)
        upBrightness = Update.Brightness(bright)
        upTemp = Update.Temperature(temp)
        upColor = Update.Color(col)
        upScene = Update.Scene(staticScene)
        upDyn = Update.Dynamic(dynamicScene, spd)
        upSpeed = Update.Speed(spd)
        upStatus = Update.Status(status)
      yield
        val encEnabled = Encoder[Update].apply(upEnabled)
        val encBrightness = Encoder[Update].apply(upBrightness)
        val encTemp = Encoder[Update].apply(upTemp)
        val encColor = Encoder[Update].apply(upColor)
        val encScene = Encoder[Update].apply(upScene)
        val encDyn = Encoder[Update].apply(upDyn)
        val encSpeed = Encoder[Update].apply(upSpeed)
        val encStatus = Encoder[Update].apply(upStatus)

        assertTrue(
          encEnabled == Json.obj("enabled" -> Json.fromBoolean(true)),
          encBrightness == Json.obj("brightness" -> Json.fromInt(50)),
          encTemp == Json.obj("temperature" -> Json.fromInt(3000)),
          encColor == Json.obj("color" -> Json.obj("red" -> Json.fromInt(255), "green" -> Json.fromInt(0), "blue" -> Json.fromInt(0))),
          encScene == Json.obj("scene" -> Json.fromInt(6)),
          encDyn == Json.obj("scene" -> Json.fromInt(3), "speed" -> Json.fromInt(100)),
          encSpeed == Json.obj("speed" -> Json.fromInt(100)),
          encStatus == Json.obj("status" -> Json.obj("temperature" -> Json.fromInt(3000), "brightness" -> Json.fromInt(50))),
          Decoder[Update].decodeJson(encEnabled) == Right(upEnabled),
          Decoder[Update.Enabled].decodeJson(encEnabled) == Right(upEnabled),
          Decoder[Update].decodeJson(encBrightness) == Right(upBrightness),
          Decoder[Update.Brightness].decodeJson(encBrightness) == Right(upBrightness),
          Decoder[Update].decodeJson(encTemp) == Right(upTemp),
          Decoder[Update.Temperature].decodeJson(encTemp) == Right(upTemp),
          Decoder[Update].decodeJson(encColor) == Right(upColor),
          Decoder[Update.Color].decodeJson(encColor) == Right(upColor),
          Decoder[Update].decodeJson(encScene) == Right(upScene),
          Decoder[Update.Scene].decodeJson(encScene) == Right(upScene),
          Decoder[Update].decodeJson(encDyn) == Right(upDyn),
          Decoder[Update.Dynamic].decodeJson(encDyn) == Right(upDyn),
          Decoder[Update].decodeJson(encSpeed) == Right(upSpeed),
          Decoder[Update.Speed].decodeJson(encSpeed) == Right(upSpeed),
          Decoder[Update].decodeJson(encStatus) == Right(upStatus),
          Decoder[Update.Status].decodeJson(encStatus) == Right(upStatus),
          Decoder[Update].decodeAccumulating(encEnabled.hcursor).isValid
        )
    },
    test("reject invalid JSON for Update") {
      val nonObject = Json.fromString("enabled")
      val emptyObj = Json.obj()
      val unknownObj = Json.obj("unknown" -> Json.fromInt(1))
      val invalidBright = Json.obj("brightness" -> Json.fromInt(5))
      assertTrue(
        Decoder[Update].decodeJson(nonObject).isLeft,
        Decoder[Update].decodeJson(emptyObj).isLeft,
        Decoder[Update].decodeJson(unknownObj).isLeft,
        Decoder[Update].decodeJson(invalidBright).isLeft,
        Decoder[Update].decodeAccumulating(nonObject.hcursor).isInvalid,
        Decoder[Update].decodeAccumulating(emptyObj.hcursor).isInvalid
      )
    }
  )
