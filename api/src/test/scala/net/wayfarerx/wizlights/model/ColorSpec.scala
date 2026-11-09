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
 * Specifications for the Color model.
 */
object ColorSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ColorSpec")(
    test("predefined color constants") {
      assertTrue(
        Color.Black.red == 0 && Color.Black.green == 0 && Color.Black.blue == 0 && Color.Black.cool == 0 && Color.Black.warm == 0,
        Color.Silver.red == 192 && Color.Silver.green == 192 && Color.Silver.blue == 192 && Color.Silver.cool == 0 && Color.Silver.warm == 0,
        Color.Gray.red == 128 && Color.Gray.green == 128 && Color.Gray.blue == 128 && Color.Gray.cool == 0 && Color.Gray.warm == 0,
        Color.White.red == 255 && Color.White.green == 255 && Color.White.blue == 255 && Color.White.cool == 0 && Color.White.warm == 0,
        Color.Maroon.red == 128 && Color.Maroon.green == 0 && Color.Maroon.blue == 0 && Color.Maroon.cool == 0 && Color.Maroon.warm == 0,
        Color.Red.red == 255 && Color.Red.green == 0 && Color.Red.blue == 0 && Color.Red.cool == 0 && Color.Red.warm == 0,
        Color.Purple.red == 128 && Color.Purple.green == 0 && Color.Purple.blue == 128 && Color.Purple.cool == 0 && Color.Purple.warm == 0,
        Color.Fuchsia.red == 255 && Color.Fuchsia.green == 0 && Color.Fuchsia.blue == 255 && Color.Fuchsia.cool == 0 && Color.Fuchsia.warm == 0,
        Color.Green.red == 0 && Color.Green.green == 128 && Color.Green.blue == 0 && Color.Green.cool == 0 && Color.Green.warm == 0,
        Color.Lime.red == 0 && Color.Lime.green == 255 && Color.Lime.blue == 0 && Color.Lime.cool == 0 && Color.Lime.warm == 0,
        Color.Olive.red == 128 && Color.Olive.green == 128 && Color.Olive.blue == 0 && Color.Olive.cool == 0 && Color.Olive.warm == 0,
        Color.Yellow.red == 255 && Color.Yellow.green == 255 && Color.Yellow.blue == 0 && Color.Yellow.cool == 0 && Color.Yellow.warm == 0,
        Color.Navy.red == 0 && Color.Navy.green == 0 && Color.Navy.blue == 128 && Color.Navy.cool == 0 && Color.Navy.warm == 0,
        Color.Blue.red == 0 && Color.Blue.green == 0 && Color.Blue.blue == 255 && Color.Blue.cool == 0 && Color.Blue.warm == 0,
        Color.Teal.red == 0 && Color.Teal.green == 128 && Color.Teal.blue == 128 && Color.Teal.cool == 0 && Color.Teal.warm == 0,
        Color.Aqua.red == 0 && Color.Aqua.green == 255 && Color.Aqua.blue == 255 && Color.Aqua.cool == 0 && Color.Aqua.warm == 0
      )

    },
    test("validate and construct colors with from") {
      val badFrom = Color.from(-1, 256, 300, -5, 260)
      assertTrue(
        Color.from(10, 20, 30).exists(c => c.red == 10 && c.green == 20 && c.blue == 30 && c.cool == 0 && c.warm == 0),
        Color.from(10, 20, 30, 40, 50).exists(c => c.red == 10 && c.green == 20 && c.blue == 30 && c.cool == 40 && c.warm == 50),
        badFrom.isLeft,
        badFrom.left.toOption.get.size == 5
      )
    },
    test("construct color effect with make") {
      for
        madeRgb <- Color.make(10, 20, 30)
        made5Ch <- Color.make(10, 20, 30, 40, 50)
        errRedLow <- Color.make(-1, 0, 0).flip
        errRedHigh <- Color.make(256, 0, 0).flip
        errGreenLow <- Color.make(0, -1, 0).flip
        errGreenHigh <- Color.make(0, 256, 0).flip
        errBlueLow <- Color.make(0, 0, -1).flip
        errBlueHigh <- Color.make(0, 0, 256).flip
        errCoolLow <- Color.make(0, 0, 0, -1, 0).flip
        errCoolHigh <- Color.make(0, 0, 0, 256, 0).flip
        errWarmLow <- Color.make(0, 0, 0, 0, -1).flip
        errWarmHigh <- Color.make(0, 0, 0, 0, 256).flip
      yield assertTrue(
        madeRgb.red == 10 && madeRgb.green == 20 && madeRgb.blue == 30 && madeRgb.cool == 0 && madeRgb.warm == 0,
        made5Ch.red == 10 && made5Ch.green == 20 && made5Ch.blue == 30 && made5Ch.cool == 40 && made5Ch.warm == 50,
        errRedLow.getMessage.contains("Invalid red color component"),
        errRedHigh.getMessage.contains("Invalid red color component"),
        errGreenLow.getMessage.contains("Invalid green color component"),
        errGreenHigh.getMessage.contains("Invalid green color component"),
        errBlueLow.getMessage.contains("Invalid blue color component"),
        errBlueHigh.getMessage.contains("Invalid blue color component"),
        errCoolLow.getMessage.contains("Invalid cool color component"),
        errCoolHigh.getMessage.contains("Invalid cool color component"),
        errWarmLow.getMessage.contains("Invalid warm color component"),
        errWarmHigh.getMessage.contains("Invalid warm color component")
      )
    },
    test("encode and decode Color as JSON") {
      val rgb = Color.from(255, 128, 0).toOption.get
      val fiveCh = Color.from(100, 150, 200, 50, 75).toOption.get
      val encRgb = Encoder[Color].apply(rgb)
      val enc5Ch = Encoder[Color].apply(fiveCh)
      val decRgb = Decoder[Color].decodeJson(encRgb)
      val dec5Ch = Decoder[Color].decodeJson(enc5Ch)
      val invalidJson = Decoder[Color].decodeJson(Json.obj("red" -> Json.fromInt(300), "green" -> Json.fromInt(0), "blue" -> Json.fromInt(0)))
      val missingFields = Decoder[Color].decodeJson(Json.obj("red" -> Json.fromInt(10)))
      assertTrue(
        encRgb == Json.obj("red" -> Json.fromInt(255), "green" -> Json.fromInt(128), "blue" -> Json.fromInt(0)),
        enc5Ch == Json.obj("red" -> Json.fromInt(100), "green" -> Json.fromInt(150), "blue" -> Json.fromInt(200), "cool" -> Json.fromInt(50), "warm" -> Json.fromInt(75)),
        decRgb == Right(rgb),
        dec5Ch == Right(fiveCh),
        invalidJson.isLeft,
        missingFields.isLeft
      )
    }
  )
