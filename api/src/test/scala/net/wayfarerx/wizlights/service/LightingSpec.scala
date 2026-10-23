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
package service

import scala.annotation.targetName

import zio.stream.UStream
import zio.{Ref, Scope, UIO, URIO, ZIO}
import zio.test.*

import model.*

/**
 * Specifications for the Lighting service trait and its default methods.
 */
object LightingSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightingSpec")(
    test("dispatch update(macAddress, Update) to the corresponding update method") {
      for
        invocations <- Ref.make(List.empty[String])
        lighting = createMockLighting(invocations)
        mac <- MacAddress.make("0123456789ab")
        bright <- Brightness.make(80)
        temp <- Temperature.make(3500)
        col <- Color.make(100, 200, 50)
        spd <- Speed.make(150)
        statScene = Scene.Static.Daylight
        dynScene = Scene.Dynamic.Ocean
        status = Status.White(temp, bright)

        _ <- lighting.update(mac, Update.Enabled(true))
        _ <- lighting.update(mac, Update.Brightness(bright))
        _ <- lighting.update(mac, Update.Temperature(temp))
        _ <- lighting.update(mac, Update.Color(col))
        _ <- lighting.update(mac, Update.Static(statScene))
        _ <- lighting.update(mac, Update.Dynamic(dynScene, spd))
        _ <- lighting.update(mac, Update.Speed(spd))
        _ <- lighting.update(mac, Update.Status(status))

        calls <- invocations.get
      yield assertTrue(
        calls.reverse == List(
          s"enabled:0123456789ab:true",
          s"brightness:0123456789ab:80",
          s"temperature:0123456789ab:3500",
          s"color:0123456789ab:100,200,50",
          s"scene:0123456789ab:Daylight",
          s"dynamic:0123456789ab:Ocean:150",
          s"speed:0123456789ab:150",
          s"status:0123456789ab:$status"
        )
      )
    },
    test("dispatch updateAll(macAddresses, Update) to the corresponding updateAll method") {
      for
        invocations <- Ref.make(List.empty[String])
        lighting = createMockLighting(invocations)
        mac1 <- MacAddress.make("0123456789ab")
        mac2 <- MacAddress.make("cdef01234567")
        macs = List(mac1, mac2)
        bright <- Brightness.make(50)
        temp <- Temperature.make(4000)
        col <- Color.make(255, 0, 0)
        spd <- Speed.make(80)
        statScene = Scene.Static.Cozy
        dynScene = Scene.Dynamic.Fireplace
        status = Status.Disabled

        _ <- lighting.updateAll(macs, Update.Enabled(false))
        _ <- lighting.updateAll(macs, Update.Brightness(bright))
        _ <- lighting.updateAll(macs, Update.Temperature(temp))
        _ <- lighting.updateAll(macs, Update.Color(col))
        _ <- lighting.updateAll(macs, Update.Static(statScene))
        _ <- lighting.updateAll(macs, Update.Dynamic(dynScene, spd))
        _ <- lighting.updateAll(macs, Update.Speed(spd))
        _ <- lighting.updateAll(macs, Update.Status(status))

        calls <- invocations.get
      yield assertTrue(
        calls.reverse == List(
          "all-enabled:2:false",
          "all-brightness:2:50",
          "all-temperature:2:4000",
          "all-color:2:255,0,0",
          "all-scene:2:Cozy",
          "all-dynamic:2:Fireplace:80",
          "all-speed:2:80",
          s"all-status:2:$status"
        )
      )
    }
  )

  private def createMockLighting(invocations: Ref[List[String]]): Lighting = new Lighting:

    override def lights: UIO[Set[Light]] = ZIO.succeed(Set.empty)

    override def get(macAddress: MacAddress): UIO[Light] =
      ZIO.succeed(Light(macAddress, None, None))

    override def getAll(macAddresses: Iterable[MacAddress]): UIO[Set[Light]] =
      ZIO.succeed(macAddresses.map(mac => Light(mac, None, None)).toSet)

    override def update(macAddress: MacAddress, enabled: Boolean): UIO[Boolean] =
      invocations.update(s"enabled:$macAddress:$enabled" :: _).as(true)

    override def updateAll(macAddresses: Iterable[MacAddress], enabled: Boolean): UIO[Int] =
      invocations.update(s"all-enabled:${macAddresses.size}:$enabled" :: _).as(macAddresses.size)

    @targetName("updateBrightness")
    override def update(macAddress: MacAddress, brightness: Brightness): UIO[Boolean] =
      invocations.update(s"brightness:$macAddress:${brightness: Int}" :: _).as(true)

    @targetName("updateAllBrightness")
    override def updateAll(macAddresses: Iterable[MacAddress], brightness: Brightness): UIO[Int] =
      invocations.update(s"all-brightness:${macAddresses.size}:${brightness: Int}" :: _).as(macAddresses.size)

    @targetName("updateTemperature")
    override def update(macAddress: MacAddress, temperature: Temperature): UIO[Boolean] =
      invocations.update(s"temperature:$macAddress:${temperature: Int}" :: _).as(true)

    @targetName("updateAllTemperature")
    override def updateAll(macAddresses: Iterable[MacAddress], temperature: Temperature): UIO[Int] =
      invocations.update(s"all-temperature:${macAddresses.size}:${temperature: Int}" :: _).as(macAddresses.size)

    override def update(macAddress: MacAddress, color: Color): UIO[Boolean] =
      invocations.update(s"color:$macAddress:${color.red},${color.green},${color.blue}" :: _).as(true)

    override def updateAll(macAddresses: Iterable[MacAddress], color: Color): UIO[Int] =
      invocations.update(s"all-color:${macAddresses.size}:${color.red},${color.green},${color.blue}" :: _).as(macAddresses.size)

    override def update(macAddress: MacAddress, scene: Scene): UIO[Boolean] =
      invocations.update(s"scene:$macAddress:${scene.name}" :: _).as(true)

    override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene): UIO[Int] =
      invocations.update(s"all-scene:${macAddresses.size}:${scene.name}" :: _).as(macAddresses.size)

    override def update(macAddress: MacAddress, scene: Scene.Dynamic, speed: Speed): UIO[Boolean] =
      invocations.update(s"dynamic:$macAddress:${scene.name}:${speed: Int}" :: _).as(true)

    override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene.Dynamic, speed: Speed): UIO[Int] =
      invocations.update(s"all-dynamic:${macAddresses.size}:${scene.name}:${speed: Int}" :: _).as(macAddresses.size)

    @targetName("updateSpeed")
    override def update(macAddress: MacAddress, speed: Speed): UIO[Boolean] =
      invocations.update(s"speed:$macAddress:${speed: Int}" :: _).as(true)

    @targetName("updateAllSpeed")
    override def updateAll(macAddresses: Iterable[MacAddress], speed: Speed): UIO[Int] =
      invocations.update(s"all-speed:${macAddresses.size}:${speed: Int}" :: _).as(macAddresses.size)

    override def update(macAddress: MacAddress, status: Status): UIO[Boolean] =
      invocations.update(s"status:$macAddress:$status" :: _).as(true)

    override def updateAll(macAddresses: Iterable[MacAddress], status: Status): UIO[Int] =
      invocations.update(s"all-status:${macAddresses.size}:$status" :: _).as(macAddresses.size)

    override def subscribe: URIO[Scope, UStream[(Light, Option[Source])]] =
      ZIO.succeed(zio.stream.ZStream.empty)
