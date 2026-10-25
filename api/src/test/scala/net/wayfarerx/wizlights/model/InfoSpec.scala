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
 * Specifications for the Info model.
 */
object InfoSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("InfoSpec")(
    test("create Info with minimal and full parameters") {
      for
        ip <- IpAddress.make("192.168.1.50")
        sig <- Signal.make(-60)
        mod <- Module.make("ESP32_WROOM")
        fw <- Firmware.make("1.25.0")
        dev <- Device.make(32)
        h <- Home.make(100)
        r <- Room.make(5)
        g <- Group.make(2)
        t <- Timestamp.make(1700000000000L)
        minInfo = Info(ip)
        fullInfo = Info(
          ipAddress = ip,
          signal = Some(sig),
          home = Some(h),
          room = Some(r),
          module = Some(mod),
          firmware = Some(fw),
          group = Some(g),
          device = Some(dev),
          timestamp = Some(t)
        )
      yield assertTrue(
        minInfo.ipAddress == ip,
        minInfo.signal.isEmpty,
        minInfo.home.isEmpty,
        minInfo.room.isEmpty,
        minInfo.module.isEmpty,
        minInfo.firmware.isEmpty,
        minInfo.group.isEmpty,
        minInfo.device.isEmpty,
        minInfo.timestamp.isEmpty,
        fullInfo.ipAddress == ip,
        fullInfo.signal.contains(sig),
        fullInfo.home.contains(h),
        fullInfo.room.contains(r),
        fullInfo.module.contains(mod),
        fullInfo.firmware.contains(fw),
        fullInfo.group.contains(g),
        fullInfo.device.contains(dev),
        fullInfo.timestamp.contains(t)
      )
    },
    test("encode and decode minimal Info as JSON") {
      for
        ip <- IpAddress.make("192.168.1.50")
        minInfo = Info(ip)
        encoded = Encoder[Info].apply(minInfo)
        decoded = Decoder[Info].decodeJson(encoded)
      yield assertTrue(
        encoded == Json.obj("ipAddress" -> Json.fromString("192.168.1.50")),
        decoded == Right(minInfo)
      )
    },
    test("encode and decode full Info as JSON") {
      for
        ip <- IpAddress.make("192.168.1.50")
        sig <- Signal.make(-60)
        mod <- Module.make("ESP32")
        fw <- Firmware.make("1.25.0")
        dev <- Device.make(32)
        h <- Home.make(100)
        r <- Room.make(5)
        g <- Group.make(2)
        t <- Timestamp.make(1700000000000L)
        fullInfo = Info(
          ipAddress = ip,
          signal = Some(sig),
          home = Some(h),
          room = Some(r),
          module = Some(mod),
          firmware = Some(fw),
          group = Some(g),
          device = Some(dev),
          timestamp = Some(t)
        )
        encoded = Encoder[Info].apply(fullInfo)
        decoded = Decoder[Info].decodeJson(encoded)
      yield assertTrue(
        decoded == Right(fullInfo)
      )
    },
    test("fail decoding when ipAddress is missing or invalid") {
      val missingIp = Json.obj("signal" -> Json.fromInt(-50))
      val invalidIp = Json.obj("ipAddress" -> Json.fromString("not-an-ip"))
      val invalidSignal = Json.obj("ipAddress" -> Json.fromString("192.168.1.1"), "signal" -> Json.fromInt(10))
      assertTrue(
        Decoder[Info].decodeJson(missingIp).isLeft,
        Decoder[Info].decodeJson(invalidIp).isLeft,
        Decoder[Info].decodeJson(invalidSignal).isLeft
      )
    }
  )
