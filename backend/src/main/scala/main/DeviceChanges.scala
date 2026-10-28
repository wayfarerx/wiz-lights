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
package main

import java.net.InetAddress

import zio.{IO, ZIO}

import model.*

/**
 * Base trait for changes detected on a device.
 */
private sealed trait DeviceChanges:

  /** The MAC address of the device. */
  def macAddress: MacAddress

  /** The timestamp when the device change was observed. */
  def timestamp: Long

/**
 * The supported device change types.
 */
private object DeviceChanges:

  /**
   * Details of settings changes reported by a light.
   *
   * @param macAddress The MAC address of the device.
   * @param timestamp  The timestamp when the device change was observed.
   * @param home       The home the light belongs to if available.
   * @param room       The room the light is in if available.
   * @param module     The module name of the light if available.
   * @param firmware   The firmware version of the light if available.
   * @param group      The group the light belongs to if defined.
   * @param device     The device type ID of the light if available.
   */
  final case class Settings(
    macAddress: MacAddress,
    timestamp: Long,
    home: Option[Home] = None,
    room: Option[Room] = None,
    module: Option[Module] = None,
    firmware: Option[Firmware] = None,
    group: Option[Group] = None,
    device: Option[Device] = None
  ) extends DeviceChanges

  /**
   * Factory for settings changes.
   */
  object Settings:

    /**
     * Creates a settings change from raw response payload components.
     *
     * @param macAddress  The MAC address of the device.
     * @param timestamp   The timestamp when the device change was observed.
     * @param homeId      The home ID if present.
     * @param roomId      The room ID if present.
     * @param moduleName  The module name if present.
     * @param fwVersion   The firmware version if present.
     * @param groupId     The group ID if present.
     * @param typeId      The device type ID if present.
     * @return A settings change if all specified components are valid.
     */
    def make(
      macAddress: String,
      timestamp: Long,
      homeId: Option[Int] = None,
      roomId: Option[Int] = None,
      moduleName: Option[String] = None,
      fwVersion: Option[String] = None,
      groupId: Option[Int] = None,
      typeId: Option[Int] = None
    ): IO[IllegalArgumentException, Settings] = for
      (mac, home, room, module, firmware, group, device) <-
        MacAddress.make(macAddress) validate
          homeId.fold(ZIO.none)(Home.make(_).asSome) validate
          roomId.fold(ZIO.none)(Room.make(_).asSome) validate
          moduleName.fold(ZIO.none)(Module.make(_).asSome) validate
          fwVersion.fold(ZIO.none)(Firmware.make(_).asSome) validate
          groupId.fold(ZIO.none)(Group.make(_).asSome) validate
          typeId.fold(ZIO.none)(Device.make(_).asSome)
    yield Settings(mac, timestamp, home, room, module, firmware, group, device)

  /**
   * Details of pilot changes reported by a light.
   *
   * @param macAddress  The MAC address of the device.
   * @param timestamp   The timestamp when the device change was observed.
   * @param signal      The signal strength of the light.
   * @param state       Whether the light is enabled.
   * @param brightness  The brightness of the light if known.
   * @param temperature The color temperature of the light if known.
   * @param scene       The scene of the light if known.
   * @param speed       The dynamic scene speed of the light if known.
   * @param color       The color of the light if known.
   */
  final case class Pilot(
    macAddress: MacAddress,
    timestamp: Long,
    signal: Signal,
    state: Boolean,
    brightness: Option[Brightness] = None,
    temperature: Option[Temperature] = None,
    scene: Option[Scene] = None,
    speed: Option[Speed] = None,
    color: Option[Color] = None
  ) extends DeviceChanges

  /**
   * Factory for pilot changes.
   */
  object Pilot:

    /**
     * Creates a pilot change from raw response payload components.
     *
     * @param macAddress The MAC address of the device.
     * @param timestamp  The timestamp when the device change was observed.
     * @param rssi       The Signal Strength Indicator.
     * @param state      Whether the light is enabled.
     * @param dimming    The brightness level if present.
     * @param sceneId    The scene ID if present.
     * @param speed      The dynamic scene speed if present.
     * @param temp       The color temperature if present.
     * @param r          The red component if present.
     * @param g          The green component if present.
     * @param b          The blue component if present.
     * @param c          The cool white component if present.
     * @param w          The warm white component if present.
     * @return A pilot change if all specified components are valid.
     */
    def make(
      macAddress: String,
      timestamp: Long,
      rssi: Int,
      state: Boolean,
      dimming: Option[Int] = None,
      sceneId: Option[Int] = None,
      speed: Option[Int] = None,
      temp: Option[Int] = None,
      r: Option[Int] = None,
      g: Option[Int] = None,
      b: Option[Int] = None,
      c: Option[Int] = None,
      w: Option[Int] = None
    ): IO[IllegalArgumentException, Pilot] = for
      (mac, signal, brightness, temperature, scene, speed, color) <-
        MacAddress.make(macAddress) validate
          Signal.make(rssi) validate
          dimming.fold(ZIO.none)(Brightness.make(_).asSome) validate
          temp.fold(ZIO.none)(Temperature.make(_).asSome) validate
          sceneId.fold(ZIO.none)(Scene.make(_).asSome) validate
          speed.fold(ZIO.none)(Speed.make(_).asSome) validate {
            if r.isDefined || g.isDefined || b.isDefined || c.isDefined || w.isDefined then
              Color.make(
                r.getOrElse(0),
                g.getOrElse(0),
                b.getOrElse(0),
                c.getOrElse(0),
                w.getOrElse(0)
              ).asSome
            else ZIO.none
          }
    yield Pilot(mac, timestamp, signal, state, brightness, temperature, scene, speed, color)
