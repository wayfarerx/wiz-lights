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

import java.net.InetAddress
import java.time.Instant

import cats.kernel.Order
import zio.Scope
import zio.test.*

/**
 * Specifications for the Light model.
 */
object LightSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightSpec")(
    test("delegate accessors when info is None") {
      for
        addr <- Address.make("0123456789ab")
      yield
        val light = Light(addr, None, None)
        assertTrue(
          light.address == addr,
          light.status.isEmpty,
          light.info.isEmpty,
          light.inetAddress.isEmpty,
          light.signal.isEmpty,
          light.module.isEmpty,
          light.firmware.isEmpty,
          light.device.isEmpty,
          light.lastSeen.isEmpty,
          light.home.isEmpty,
          light.room.isEmpty,
          light.group.isEmpty
        )
    },
    test("delegate accessors when info and status are present") {
      for
        addr <- Address.make("0123456789ab")
        inet <- zio.ZIO.attempt(InetAddress.getByName("192.168.1.50"))
        sig <- Signal.make(-45)
        mod <- Module.make("ESP01_SHRGB_03")
        fw <- Firmware.make("1.26.0")
        dev <- Device.make(32)
        now = Instant.parse("2026-10-18T15:30:00Z")
        h <- Home.make(100)
        r <- Room.make(10)
        g <- Group.make(3)
        temp <- Temperature.make(2700)
        bright <- Brightness.make(80)
      yield
        val info = Info(inet, sig, mod, fw, dev, now, h, r, Some(g))
        val status = Status.White(temp, bright)
        val light = Light(addr, Some(status), Some(info))
        assertTrue(
          light.address == addr,
          light.status.contains(status),
          light.info.contains(info),
          light.inetAddress.contains(inet),
          light.signal.contains(sig),
          light.module.contains(mod),
          light.firmware.contains(fw),
          light.device.contains(dev),
          light.lastSeen.contains(now),
          light.home.contains(h),
          light.room.contains(r),
          light.group.contains(g)
        )
    },
    test("support ordering and cats order by address") {
      for
        addr1 <- Address.make("000000000001")
        addr2 <- Address.make("000000000002")
      yield
        val light1 = Light(addr1, None, None)
        val light2 = Light(addr2, None, None)
        assertTrue(
          Ordering[Light].compare(light1, light2) < 0,
          Ordering[Light].compare(light2, light1) > 0,
          Ordering[Light].compare(light1, light1) == 0,
          Order[Light].compare(light1, light2) < 0,
          Order[Light].eqv(light1, light1)
        )
    }
  )
