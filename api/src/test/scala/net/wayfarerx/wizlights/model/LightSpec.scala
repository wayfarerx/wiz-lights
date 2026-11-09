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

import zio.Scope
import zio.test.*

/**
 * Specifications for the Light model.
 */
object LightSpec extends ZIOSpecDefault:

  private val testMac = MacAddress.from("0123456789ab").toOption.get
  private val testMac1 = MacAddress.from("aaaaaaaaaaaa").toOption.get
  private val testMac2 = MacAddress.from("bbbbbbbbbbbb").toOption.get
  private val testIp = IpAddress.from("192.168.1.50").toOption.get
  private val testSig = Signal.Fair
  private val testMod = Module.from("ESP32").toOption.get
  private val testFw = Firmware.from("1.0.0").toOption.get
  private val testDev = Device.from(1).toOption.get
  private val testHome = Home.from(10).toOption.get
  private val testRoom = Room.from(3).toOption.get
  private val testGroup = Group.from(4).toOption.get

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightSpec")(
    test("delegate info fields correctly when info is present and absent") {
      val info = Info(
        ipAddress = testIp,
        signal = Some(testSig),
        module = Some(testMod),
        firmware = Some(testFw),
        device = Some(testDev),
        home = Some(testHome),
        room = Some(testRoom),
        group = Some(testGroup)
      )
      val lightWithoutInfo = Light(testMac, None, None)
      val lightWithInfo = Light(testMac, Some(Status.Disabled), Some(info))
      assertTrue(
        lightWithoutInfo.ipAddress.isEmpty,
        lightWithoutInfo.signal.isEmpty,
        lightWithoutInfo.module.isEmpty,
        lightWithoutInfo.firmware.isEmpty,
        lightWithoutInfo.device.isEmpty,
        lightWithoutInfo.home.isEmpty,
        lightWithoutInfo.room.isEmpty,
        lightWithoutInfo.group.isEmpty,
        lightWithInfo.ipAddress.contains(testIp),
        lightWithInfo.signal.contains(testSig),
        lightWithInfo.module.contains(testMod),
        lightWithInfo.firmware.contains(testFw),
        lightWithInfo.device.contains(testDev),
        lightWithInfo.home.contains(testHome),
        lightWithInfo.room.contains(testRoom),
        lightWithInfo.group.contains(testGroup)
      )
    },
    test("order lights by MAC address using Ordering and cats Order") {
      val light1 = Light(testMac1, None, None)
      val light2 = Light(testMac2, None, None)
      assertTrue(
        Ordering[Light].compare(light1, light2) < 0,
        Ordering[Light].compare(light2, light1) > 0,
        Ordering[Light].compare(light1, light1) == 0,
        Order[Light].compare(light1, light2) < 0,
        Order[Light].eqv(light1, light1)
      )
    },
    test("encode and decode Light as JSON") {
      val lightMinimal = Light(testMac, None, None)
      val encodedMin = Encoder[Light].apply(lightMinimal)
      val decodedMin = Decoder[Light].decodeJson(encodedMin)
      val ip = IpAddress.from("192.168.1.1").toOption.get
      val lightFull = Light(testMac, Some(Status.Disabled), Some(Info(ip)))
      val encodedFull = Encoder[Light].apply(lightFull)
      val decodedFull = Decoder[Light].decodeJson(encodedFull)
      val missingMac = Decoder[Light].decodeJson(Json.obj())
      val invalidMac = Decoder[Light].decodeJson(Json.obj("macAddress" -> Json.fromString("bad-mac")))
      assertTrue(
        encodedMin == Json.obj("macAddress" -> Json.fromString("0123456789ab")),
        decodedMin == Right(lightMinimal),
        decodedFull == Right(lightFull),
        missingMac.isLeft,
        invalidMac.isLeft,
        Decoder[Light].decodeAccumulating(encodedMin.hcursor).isValid
      )
    }
  )
