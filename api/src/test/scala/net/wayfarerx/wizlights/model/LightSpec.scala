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

import io.circe.{Decoder, Encoder, Json}

import zio.{Scope, ZIO}
import zio.test.*

/**
 * Specifications for the Light model.
 */
object LightSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightSpec")(
    test("delegate info fields correctly when info is present and absent") {
      for
        mac <- MacAddress.make("0123456789ab")
        ip <- IpAddress.make("192.168.1.50")
        sig <- Signal.make(-70)
        mod <- Module.make("ESP32")
        fw <- Firmware.make("1.0.0")
        dev <- Device.make(1)
        h <- Home.make(10)
        r <- Room.make(3)
        g <- Group.make(4)
        info = Info(
          ipAddress = ip,
          signal = Some(sig),
          module = Some(mod),
          firmware = Some(fw),
          device = Some(dev),
          lastSeen = Some(1700000000000L),
          home = Some(h),
          room = Some(r),
          group = Some(g)
        )
        lightWithoutInfo = Light(mac, None, None)
        lightWithInfo = Light(mac, Some(Status.Disabled), Some(info))
      yield assertTrue(
        lightWithoutInfo.ipAddress.isEmpty,
        lightWithoutInfo.signal.isEmpty,
        lightWithoutInfo.module.isEmpty,
        lightWithoutInfo.firmware.isEmpty,
        lightWithoutInfo.device.isEmpty,
        lightWithoutInfo.lastSeen.isEmpty,
        lightWithoutInfo.home.isEmpty,
        lightWithoutInfo.room.isEmpty,
        lightWithoutInfo.group.isEmpty,
        lightWithInfo.ipAddress.contains(ip),
        lightWithInfo.signal.contains(sig),
        lightWithInfo.module.contains(mod),
        lightWithInfo.firmware.contains(fw),
        lightWithInfo.device.contains(dev),
        lightWithInfo.lastSeen.contains(1700000000000L),
        lightWithInfo.home.contains(h),
        lightWithInfo.room.contains(r),
        lightWithInfo.group.contains(g)
      )
    },
    test("order lights by MAC address using Ordering and cats Order") {
      for
        mac1 <- MacAddress.make("aaaaaaaaaaaa")
        mac2 <- MacAddress.make("bbbbbbbbbbbb")
        light1 = Light(mac1, None, None)
        light2 = Light(mac2, None, None)
      yield assertTrue(
        Ordering[Light].compare(light1, light2) < 0,
        Ordering[Light].compare(light2, light1) > 0,
        Ordering[Light].compare(light1, light1) == 0,
        Order[Light].compare(light1, light2) < 0,
        Order[Light].eqv(light1, light1)
      )
    },
    test("encode and decode Light as JSON") {
      for
        mac <- MacAddress.make("0123456789ab")
        lightMinimal = Light(mac, None, None)
        encodedMin = Encoder[Light].apply(lightMinimal)
        decodedMin = Decoder[Light].decodeJson(encodedMin)
        ip <- IpAddress.make("192.168.1.1")
        lightFull = Light(mac, Some(Status.Disabled), Some(Info(ip)))
        encodedFull = Encoder[Light].apply(lightFull)
        decodedFull = Decoder[Light].decodeJson(encodedFull)
        missingMac = Decoder[Light].decodeJson(Json.obj())
        invalidMac = Decoder[Light].decodeJson(Json.obj("macAddress" -> Json.fromString("bad-mac")))
      yield assertTrue(
        encodedMin == Json.obj("macAddress" -> Json.fromString("0123456789ab")),
        decodedMin == Right(lightMinimal),
        decodedFull == Right(lightFull),
        missingMac.isLeft,
        invalidMac.isLeft
      )
    }
  )
