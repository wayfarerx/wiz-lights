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
 * Specifications for the Status model.
 */
object StatusSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("StatusSpec")(
    test("update brightness on enabled statuses using withBrightness") {
      for
        b1 <- Brightness.make(30)
        b2 <- Brightness.make(80)
        temp <- Temperature.make(3000)
        spd <- Speed.make(50)
        col <- Color.make(255, 100, 50)
        white = Status.White(temp, b1)
        stat = Status.Defined.Static(Scene.Static.Cozy, b1)
        dyn = Status.Defined.Dynamic(Scene.Dynamic.Party, spd, b1)
        cust = Status.Custom(col, b1)
      yield assertTrue(
        white.withBrightness(b2) == Status.White(temp, b2),
        stat.withBrightness(b2) == Status.Defined.Static(Scene.Static.Cozy, b2),
        dyn.withBrightness(b2) == Status.Defined.Dynamic(Scene.Dynamic.Party, spd, b2),
        cust.withBrightness(b2) == Status.Custom(col, b2)
      )
    },
    test("encode and decode Status.Disabled as JSON") {
      val disabled = Status.Disabled
      val enc = Encoder[Status].apply(disabled)
      val dec = Decoder[Status].decodeJson(enc)
      val decDisabledDirect = Decoder[Status.Disabled.type].decodeJson(Json.obj())
      val invalidDisabled = Decoder[Status.Disabled.type].decodeJson(Json.obj("unexpected" -> Json.fromInt(1)))
      assertTrue(
        enc == Json.obj(),
        dec == Right(disabled),
        decDisabledDirect == Right(disabled),
        invalidDisabled.isLeft
      )
    },
    test("encode and decode Status.White as JSON") {
      for
        temp <- Temperature.make(4000)
        bright <- Brightness.make(75)
        white = Status.White(temp, bright)
        enc = Encoder[Status].apply(white)
        dec = Decoder[Status].decodeJson(enc)
        decWhite = Decoder[Status.White].decodeJson(enc)
      yield assertTrue(
        enc == Json.obj("temperature" -> Json.fromInt(4000), "brightness" -> Json.fromInt(75)),
        dec == Right(white),
        decWhite == Right(white)
      )
    },
    test("encode and decode Status.Defined.Static as JSON") {
      for
        bright <- Brightness.make(60)
        scene = Scene.Static.WarmWhite
        staticStatus = Status.Defined.Static(scene, bright)
        enc = Encoder[Status].apply(staticStatus)
        dec = Decoder[Status].decodeJson(enc)
        decDefined = Decoder[Status.Defined].decodeJson(enc)
        decStatic = Decoder[Status.Defined.Static].decodeJson(enc)
      yield assertTrue(
        enc == Json.obj("scene" -> Json.fromInt(11), "brightness" -> Json.fromInt(60)),
        dec == Right(staticStatus),
        decDefined == Right(staticStatus),
        decStatic == Right(staticStatus)
      )
    },
    test("encode and decode Status.Defined.Dynamic as JSON") {
      for
        bright <- Brightness.make(90)
        spd <- Speed.make(120)
        scene = Scene.Dynamic.Sunset
        dynStatus = Status.Defined.Dynamic(scene, spd, bright)
        enc = Encoder[Status].apply(dynStatus)
        dec = Decoder[Status].decodeJson(enc)
        decDefined = Decoder[Status.Defined].decodeJson(enc)
        decDynamic = Decoder[Status.Defined.Dynamic].decodeJson(enc)
      yield assertTrue(
        enc == Json.obj("scene" -> Json.fromInt(3), "speed" -> Json.fromInt(120), "brightness" -> Json.fromInt(90)),
        dec == Right(dynStatus),
        decDefined == Right(dynStatus),
        decDynamic == Right(dynStatus)
      )
    },
    test("encode and decode Status.Custom as JSON") {
      for
        bright <- Brightness.make(85)
        col <- Color.make(255, 0, 128)
        custStatus = Status.Custom(col, bright)
        enc = Encoder[Status].apply(custStatus)
        dec = Decoder[Status].decodeJson(enc)
        decCustom = Decoder[Status.Custom].decodeJson(enc)
      yield assertTrue(
        dec == Right(custStatus),
        decCustom == Right(custStatus)
      )
    },
    test("reject invalid JSON for Status and Status.Enabled") {
      val nonObject = Json.fromString("disabled")
      val missingDiscrim = Json.obj("brightness" -> Json.fromInt(50))
      val invalidTemp = Json.obj("temperature" -> Json.fromInt(100), "brightness" -> Json.fromInt(50))
      assertTrue(
        Decoder[Status].decodeJson(nonObject).isLeft,
        Decoder[Status].decodeJson(missingDiscrim).isLeft,
        Decoder[Status].decodeJson(invalidTemp).isLeft,
        Decoder[Status.Enabled].decodeJson(missingDiscrim).isLeft
      )
    }
  )
