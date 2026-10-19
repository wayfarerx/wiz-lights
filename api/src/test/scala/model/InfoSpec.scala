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

import zio.Scope
import zio.test.*

/**
 * Specifications for the Info model.
 */
object InfoSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("InfoSpec")(
    test("successfully create Info without group") {
      for
        inet <- zio.ZIO.attempt(InetAddress.getByName("192.168.1.100"))
        sig <- Signal.make(-60)
        mod <- Module.make("ESP01_SHRGB_03")
        fw <- Firmware.make("1.26.0")
        dev <- Device.make(32)
        now = Instant.parse("2026-10-18T12:00:00Z")
        h <- Home.make(12345)
        r <- Room.make(1)
      yield
        val info = Info(inet, sig, mod, fw, dev, now, h, r)
        assertTrue(
          info.inetAddress == inet,
          info.signal == sig,
          info.module == mod,
          info.firmware == fw,
          info.device == dev,
          info.lastSeen == now,
          info.home == h,
          info.room == r,
          info.group.isEmpty
        )
    },
    test("successfully create Info with optional group") {
      for
        inet <- zio.ZIO.attempt(InetAddress.getByName("192.168.1.101"))
        sig <- Signal.make(-50)
        mod <- Module.make("ESP01_SHRGB_03")
        fw <- Firmware.make("1.26.0")
        dev <- Device.make(32)
        now = Instant.now()
        h <- Home.make(12345)
        r <- Room.make(1)
        g <- Group.make(2)
      yield
        val info = Info(inet, sig, mod, fw, dev, now, h, r, Some(g))
        assertTrue(
          info.inetAddress == inet,
          info.signal == sig,
          info.module == mod,
          info.firmware == fw,
          info.device == dev,
          info.lastSeen == now,
          info.home == h,
          info.room == r,
          info.group.contains(g)
        )
    }
  )
