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

import zio.Scope
import zio.test.*

/**
 * Specifications for the Info model.
 */
object InfoSpec extends ZIOSpecDefault:

  private val testIp = IpAddress.from("192.168.1.50").toOption.get
  private val testSig = Signal.Fair
  private val testMod = Module.from("ESP32_WROOM").toOption.get
  private val testFw = Firmware.from("1.25.0").toOption.get
  private val testDev = Device.from(32).toOption.get
  private val testHome = Home.from(100).toOption.get
  private val testRoom = Room.from(5).toOption.get
  private val testGroup = Group.from(2).toOption.get

  private val minInfo = Info(testIp)
  private val fullInfo = Info(
    ipAddress = testIp,
    signal = Some(testSig),
    home = Some(testHome),
    room = Some(testRoom),
    module = Some(testMod),
    firmware = Some(testFw),
    group = Some(testGroup),
    device = Some(testDev)
  )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("InfoSpec")(
    test("create Info with minimal and full parameters") {
      assertTrue(
        minInfo.ipAddress == testIp,
        minInfo.signal.isEmpty,
        minInfo.home.isEmpty,
        minInfo.room.isEmpty,
        minInfo.module.isEmpty,
        minInfo.firmware.isEmpty,
        minInfo.group.isEmpty,
        minInfo.device.isEmpty,
        fullInfo.ipAddress == testIp,
        fullInfo.signal.contains(testSig),
        fullInfo.home.contains(testHome),
        fullInfo.room.contains(testRoom),
        fullInfo.module.contains(testMod),
        fullInfo.firmware.contains(testFw),
        fullInfo.group.contains(testGroup),
        fullInfo.device.contains(testDev)
      )
    },
    test("encode and decode minimal Info as JSON") {
      val encoded = Encoder[Info].apply(minInfo)
      val decoded = Decoder[Info].decodeJson(encoded)
      assertTrue(
        encoded == Json.obj("ipAddress" -> Json.fromString("192.168.1.50")),
        decoded == Right(minInfo),
        Decoder[Info].decodeAccumulating(encoded.hcursor).isValid
      )
    },
    test("encode and decode full Info as JSON") {
      val encoded = Encoder[Info].apply(fullInfo)
      val decoded = Decoder[Info].decodeJson(encoded)
      assertTrue(
        decoded == Right(fullInfo),
        Decoder[Info].decodeAccumulating(encoded.hcursor).isValid
      )
    },
    test("fail decoding when ipAddress is missing or invalid") {
      val missingIp = Json.obj("signal" -> Json.fromInt(-50))
      val invalidIp = Json.obj("ipAddress" -> Json.fromString("not-an-ip"))
      val invalidSignal = Json.obj("ipAddress" -> Json.fromString("192.168.1.1"), "signal" -> Json.fromString("invalid"))
      assertTrue(
        Decoder[Info].decodeJson(missingIp).isLeft,
        Decoder[Info].decodeJson(invalidIp).isLeft,
        Decoder[Info].decodeJson(invalidSignal).isLeft,
        Decoder[Info].decodeAccumulating(missingIp.hcursor).isInvalid
      )
    }
  )
