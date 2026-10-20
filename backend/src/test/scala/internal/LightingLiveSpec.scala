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
package backend
package internal

import java.net.InetAddress
import java.time.Instant

import scala.concurrent.duration.*

import zio.{Hub, Queue, Scope}
import zio.test.*

import model.*
import network.{Incoming, Network, Outgoing}
import protocol.*
import service.Update

/**
 * Specifications for the LightingLive service implementation.
 */
object LightingLiveSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightingLiveSpec")(
    test("extractStatus parses Disabled state") {
      for
        status <- LightingLive.extractStatus(false, None, None, None, None, None, None, None, None, None)
      yield assertTrue(status == Status.Disabled)
    },
    test("extractStatus parses White state") {
      for
        status <- LightingLive.extractStatus(true, Some(80), None, None, Some(2700), None, None, None, None, None)
      yield assertTrue(
        status match
          case Status.White(t, b) => (t: Int) == 2700 && (b: Int) == 80
          case _ => false
      )
    },
    test("extractStatus parses Defined Dynamic state") {
      for
        status <- LightingLive.extractStatus(true, Some(90), Some(1), Some(120), None, None, None, None, None, None)
      yield assertTrue(
        status match
          case Status.Defined.Dynamic(s, sp, b) => s == Scene.Ocean && (sp: Int) == 120 && (b: Int) == 90
          case _ => false
      )
    },
    test("extractStatus parses Defined Static state") {
      for
        status <- LightingLive.extractStatus(true, Some(70), Some(1000), None, None, None, None, None, None, None)
      yield assertTrue(
        status match
          case Status.Defined.Static(s, b) => s == Scene.Rhythm && (b: Int) == 70
          case _ => false
      )
    },
    test("extractStatus parses Custom state") {
      for
        status <- LightingLive.extractStatus(true, Some(100), None, None, None, Some(255), Some(128), Some(64), Some(32), Some(16))
      yield assertTrue(
        status match
          case Status.Custom(c, b) =>
            c.red == 255 && c.green == 128 && c.blue == 64 && c.cool == 32 && c.warm == 16 && (b: Int) == 100
          case _ => false
      )
    },
    test("parseSignal clamps out of range RSSI") {
      for
        weakest <- LightingLive.parseSignal(-100)
        strongest <- LightingLive.parseSignal(-10)
        valid <- LightingLive.parseSignal(-55)
      yield assertTrue(
        weakest == Signal.Weakest,
        strongest == Signal.Strongest,
        (valid: Int) == -55
      )
    },
    test("Record converts to Light correctly with and without Info") {
      for
        addr <- Address.make("0123456789ab")
        ip = InetAddress.getByName("192.168.1.50")
        emptyRecord = LightingLive.Record(addr)
        sig <- Signal.make(-60)
        mod <- Module.make("ESP01_SHRGB_03")
        fw <- Firmware.make("1.26.0")
        dev <- Device.make(32)
        home <- Home.make(100)
        room <- Room.make(1)
        now = Instant.now()
        fullRecord = LightingLive.Record(
          address = addr,
          inetAddress = Some(ip),
          status = Some(Status.Disabled),
          signal = Some(sig),
          module = Some(mod),
          firmware = Some(fw),
          device = Some(dev),
          lastSeen = Some(now),
          home = Some(home),
          room = Some(room)
        )
      yield
        val lightEmpty = emptyRecord.toLight
        val lightFull = fullRecord.toLight
        assertTrue(
          lightEmpty.address == addr,
          lightEmpty.info.isEmpty,
          lightFull.address == addr,
          lightFull.status.contains(Status.Disabled),
          lightFull.info.nonEmpty,
          lightFull.inetAddress.contains(ip),
          lightFull.signal.contains(sig),
          lightFull.module.contains(mod),
          lightFull.firmware.contains(fw),
          lightFull.device.contains(dev),
          lightFull.home.contains(home),
          lightFull.room.contains(room),
          lightFull.lastSeen.contains(now)
        )
    },
    test("handleIncoming processes FirstBeatResponse and GetPilotResponse") {
      for
        outgoing <- Queue.unbounded[Outgoing]
        incoming <- Hub.unbounded[Incoming]
        network = Network(outgoing, incoming)
        config <- Configuration.make(discoveryInterval = 100.seconds)
        service <- LightingLive.make(config, network)
        ip = InetAddress.getByName("192.168.1.100")
        firstBeatPayload = FirstBeatResponsePayload(
          mac = "0123456789ab",
          homeId = Some(10),
          roomId = Some(1),
          moduleName = Some("ESP01_SHRGB_03"),
          fwVersion = Some("1.26.0"),
          typeId = Some(32)
        )
        _ <- service.handleIncoming(Incoming(ip, FirstBeatResponse(Right(firstBeatPayload))))
        addr <- Address.make("0123456789ab")
        lightAfterFirstBeat <- service.get(addr)
        pilotPayload = GetPilotResponsePayload(
          mac = "0123456789ab",
          rssi = -50,
          state = true,
          dimming = Some(80),
          temp = Some(3000)
        )
        _ <- service.handleIncoming(Incoming(ip, GetPilotResponse(Right(pilotPayload))))
        lightAfterPilot <- service.get(addr)
      yield assertTrue(
        lightAfterFirstBeat.address == addr,
        lightAfterFirstBeat.inetAddress.contains(ip),
        lightAfterFirstBeat.module.isDefined,
        lightAfterPilot.address == addr,
        lightAfterPilot.status.isDefined,
        lightAfterPilot.signal.isDefined,
        lightAfterPilot.info.isDefined
      )
    },
    test("handleIncoming processes SyncPilotResponse and emits to subscriber") {
      for
        outgoing <- Queue.unbounded[Outgoing]
        incoming <- Hub.unbounded[Incoming]
        network = Network(outgoing, incoming)
        config <- Configuration.make(discoveryInterval = 100.seconds)
        service <- LightingLive.make(config, network)
        stream <- service.subscribe
        eventsPromise <- zio.Promise.make[Nothing, (Light, Option[Source])]
        _ <- stream.take(1).foreach(eventsPromise.succeed).forkScoped
        ip = InetAddress.getByName("192.168.1.101")
        syncPilotPayload = SyncPilotResponsePayload(
          mac = "a1b2c3d4e5f6",
          rssi = -65,
          state = true,
          src = Some("wzm"),
          dimming = Some(95),
          temp = Some(4000)
        )
        _ <- service.handleIncoming(Incoming(ip, SyncPilotResponse(Right(syncPilotPayload))))
        event <- eventsPromise.await
        addr <- Address.make("a1b2c3d4e5f6")
      yield assertTrue(
        event._1.address == addr,
        event._2.contains(Source.Remote),
        event._1.status.isDefined
      )
    },
    test("update sends SetPilotRequest and updates state") {
      for
        outgoing <- Queue.unbounded[Outgoing]
        incoming <- Hub.unbounded[Incoming]
        network = Network(outgoing, incoming)
        config <- Configuration.make(discoveryInterval = 100.seconds, retryBackoffs = Nil)
        service <- LightingLive.make(config, network)
        ip = InetAddress.getByName("192.168.1.102")
        pilotPayload = GetPilotResponsePayload(
          mac = "112233445566",
          rssi = -60,
          state = false
        )
        _ <- service.handleIncoming(Incoming(ip, GetPilotResponse(Right(pilotPayload))))
        addr <- Address.make("112233445566")
        _ <- outgoing.takeAll
        // Update unknown address should return false
        unknownAddr <- Address.make("999999999999")
        unknownResult <- service.update(unknownAddr, true)
        // Update known address should succeed
        knownResult <- service.update(addr, true)
        // Check updated status
        updatedLight <- service.get(addr)
        // Verify outgoing request was published
        sent <- outgoing.take
        // Test updateAll
        updateAllCount <- service.updateAll(List(addr, unknownAddr), false)
      yield assertTrue(
        !unknownResult,
        knownResult,
        updatedLight.status.contains(Status.White(Temperature.Medium, Brightness.Brightest)),
        sent match
          case Outgoing.Send(targets, SetPilotRequest(Some(state), _, _, _, _, _, _, _, _, _)) =>
            targets == Set(ip) && state
          case _ => false
        ,
        updateAllCount == 1
      )
    },
    test("partial updates: brightness, temperature, color, scene, speed, and Update ADT") {
      for
        outgoing <- Queue.unbounded[Outgoing]
        incoming <- Hub.unbounded[Incoming]
        network = Network(outgoing, incoming)
        config <- Configuration.make(discoveryInterval = 100.seconds, retryBackoffs = Nil)
        service <- LightingLive.make(config, network)
        ip = InetAddress.getByName("192.168.1.103")
        addr <- Address.make("aabbccddeeff")
        _ <- service.handleIncoming(Incoming(ip, GetPilotResponse(Right(GetPilotResponsePayload(
          mac = "aabbccddeeff",
          rssi = -55,
          state = true,
          dimming = Some(50),
          temp = Some(3000)
        )))))
        t3000 <- Temperature.make(3000)
        b80 <- Brightness.make(80)
        _ <- service.update(addr, b80)
        lightAfterDimming <- service.get(addr)
        t5000 <- Temperature.make(5000)
        _ <- service.update(addr, t5000)
        lightAfterTemp <- service.get(addr)
        col <- Color.make(255, 0, 128)
        _ <- service.update(addr, col)
        lightAfterColor <- service.get(addr)
        _ <- service.update(addr, Scene.Ocean)
        lightAfterScene <- service.get(addr)
        spd150 <- Speed.make(150)
        _ <- service.update(addr, Scene.Sunset, spd150)
        lightAfterSceneSpeed <- service.get(addr)
        spd180 <- Speed.make(180)
        _ <- service.update(addr, spd180)
        lightAfterSpeed <- service.get(addr)
        // Test overloaded update(address, Update)
        b90 <- Brightness.make(90)
        _ <- service.update(addr, Update.Brightness(b90))
        lightAfterUpdateADT <- service.get(addr)
      yield assertTrue(
        lightAfterDimming.status.contains(Status.White(t3000, b80)),
        lightAfterTemp.status.contains(Status.White(t5000, b80)),
        lightAfterColor.status.contains(Status.Custom(col, b80)),
        lightAfterScene.status match
          case Some(Status.Defined.Dynamic(s, _, _)) => s == Scene.Ocean
          case _ => false
        ,
        lightAfterSceneSpeed.status.contains(Status.Defined.Dynamic(Scene.Sunset, spd150, b80)),
        lightAfterSpeed.status.contains(Status.Defined.Dynamic(Scene.Sunset, spd180, b80)),
        lightAfterUpdateADT.status.contains(Status.Defined.Dynamic(Scene.Sunset, spd180, b90))
      )
    },
    test("retries track responding devices and stop retrying those devices") {
      for
        outgoing <- Queue.unbounded[Outgoing]
        incoming <- Hub.unbounded[Incoming]
        network = Network(outgoing, incoming)
        config <- Configuration.make(
          discoveryInterval = 100.seconds,
          retryBackoffs = List(50.milliseconds, 100.milliseconds)
        )
        service <- LightingLive.make(config, network)
        ip1 = InetAddress.getByName("192.168.1.111")
        ip2 = InetAddress.getByName("192.168.1.112")
        addr1 <- Address.make("111111111111")
        addr2 <- Address.make("222222222222")
        _ <- service.handleIncoming(Incoming(ip1, GetPilotResponse(Right(GetPilotResponsePayload(
          mac = "111111111111",
          rssi = -50,
          state = false
        )))))
        _ <- service.handleIncoming(Incoming(ip2, GetPilotResponse(Right(GetPilotResponsePayload(
          mac = "222222222222",
          rssi = -50,
          state = false
        )))))
        _ <- outgoing.takeAll
        // Send update to both lights
        _ <- service.updateAll(List(addr1, addr2), true)
        // Initial outgoing send message should target both ip1 and ip2
        initialSend <- outgoing.take
        // ip1 responds with SetPilotResponse before first retry
        _ <- service.handleIncoming(Incoming(ip1, SetPilotResponse(Right(SetPilotResponsePayload(true)))))
        // Advance clock past first backoff (50ms)
        _ <- TestClock.adjust(zio.Duration.fromScala(50.milliseconds))
        // First retry message should only target ip2
        retry1 <- outgoing.take
        // ip2 responds before second retry
        _ <- service.handleIncoming(Incoming(ip2, SetPilotResponse(Right(SetPilotResponsePayload(true)))))
        // Advance clock past second backoff (100ms)
        _ <- TestClock.adjust(zio.Duration.fromScala(100.milliseconds))
        // No further messages should be sent since all devices responded
        noMoreMessages <- outgoing.poll
      yield assertTrue(
        initialSend match
          case Outgoing.Send(targets, _) => targets == Set(ip1, ip2)
          case _ => false
        ,
        retry1 match
          case Outgoing.Send(targets, _) => targets == Set(ip2)
          case _ => false
        ,
        noMoreMessages.isEmpty
      )
    }
  )
