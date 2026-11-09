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

import zio.{IO, ZIO}

import model.*
import service.Update

/**
 * Base trait for changes detected on a device.
 */
private sealed trait Changes:

  /** The MAC address of the device. */
  def macAddress: MacAddress

  /** The timestamp when the device change was observed. */
  def timestamp: Long

/**
 * The supported device change types.
 */
private object Changes:

  /**
   * Marks the registration of a device.
   *
   * @param macAddress The MAC address of the device.
   * @param timestamp  The timestamp when the registration was observed.
   */
  case class Registered(
    macAddress: MacAddress,
    timestamp: Long
  ) extends Changes

  /**
   * Details of an update submitted to the service.
   *
   * @param macAddress The MAC address of the device.
   * @param timestamp  The timestamp when the device change was observed.
   * @param update     The update that was submitted by the service.
   */
  case class Updates(
    macAddress: MacAddress,
    timestamp: Long,
    update: Update
  ) extends Changes

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
  case class Settings private(
    macAddress: MacAddress,
    timestamp: Long,
    home: Option[Option[Home]] = None,
    room: Option[Option[Room]] = None,
    module: Option[Module] = None,
    firmware: Option[Firmware] = None,
    group: Option[Option[Group]] = None,
    device: Option[Device] = None
  ) extends Changes

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
          parseAssignment(homeId, Home.make) validate
          parseAssignment(roomId, Room.make) validate
          moduleName.fold[IO[IllegalArgumentException, Option[Module]]](ZIO.none)(m => Module.make(m).asSome.catchAll(_ => ZIO.none)) validate
          fwVersion.fold[IO[IllegalArgumentException, Option[Firmware]]](ZIO.none)(v => Firmware.make(v).asSome.catchAll(_ => ZIO.none)) validate
          parseAssignment(groupId, Group.make) validate
          typeId.fold[IO[IllegalArgumentException, Option[Device]]](ZIO.none)(t => Device.make(t).asSome.catchAll(_ => ZIO.none))
    yield Settings(mac, timestamp, home, room, module, firmware, group, device)

    /** Parses an assignment ID, mapping None to None, <= 0 to Some(None), and positive IDs to Some(Some(T)). */
    private def parseAssignment[T](
      id: Option[Int],
      make: Int => IO[IllegalArgumentException, T]
    ): IO[IllegalArgumentException, Option[Option[T]]] =
      id.fold[IO[IllegalArgumentException, Option[Option[T]]]](ZIO.none) {
        case id if id <= 0 => ZIO.succeed(Some(None))
        case other => make(other).asSome.map(Some(_)).catchAll(_ => ZIO.succeed(Some(None)))
      }

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
  case class Pilot private(
    macAddress: MacAddress,
    timestamp: Long,
    signal: Signal,
    state: Boolean,
    brightness: Option[Brightness] = None,
    temperature: Option[Temperature] = None,
    scene: Option[Scene] = None,
    speed: Option[Speed] = None,
    color: Option[Color] = None,
    source: Option[Source] = None
  ) extends Changes

  /**
   * Factory for pilot changes.
   */
  object Pilot:

    /**
     * Creates a pilot change from raw response payload components.
     *
     * @param macAddress  The MAC address of the device.
     * @param timestamp   The timestamp when the device change was observed.
     * @param rssi        The Signal Strength Indicator.
     * @param state       Whether the light is enabled.
     * @param dimming     The brightness level if present.
     * @param sceneId     The scene ID if present.
     * @param speed       The dynamic scene speed if present.
     * @param temp        The color temperature if present.
     * @param r           The red component if present.
     * @param g           The green component if present.
     * @param b           The blue component if present.
     * @param c           The cool white component if present.
     * @param w           The warm white component if present.
     * @param src         The optional source of the change.
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
      w: Option[Int] = None,
      src: Option[String] = None
    ): IO[IllegalArgumentException, Pilot] = for
      (mac, signal, brightness, temperature, scene, speed, color, source) <-
        MacAddress.make(macAddress) validate
          Signal.make(rssi) validate
          dimming.fold(ZIO.none)(d => Brightness.make(Math.max(Brightness.Dimmest, Math.min(Brightness.Brightest, d))).asSome) validate
          temp.filter(_ > 0).fold(ZIO.none)(t => Temperature.make(Math.max(Temperature.Warmest, Math.min(Temperature.Coolest, t))).asSome) validate
          sceneId.filter(_ > 0).fold[IO[IllegalArgumentException, Option[Scene]]](ZIO.none)(id => Scene.make(id).asSome.catchAll(_ => ZIO.none)) validate
          speed.filter(_ > 0).fold(ZIO.none)(s => Speed.make(Math.max(Speed.Slowest, Math.min(Speed.Fastest, s))).asSome) validate {
            if r.isDefined || g.isDefined || b.isDefined || c.isDefined || w.isDefined then
              Color.make(
                Math.max(0, Math.min(255, r.getOrElse(0))),
                Math.max(0, Math.min(255, g.getOrElse(0))),
                Math.max(0, Math.min(255, b.getOrElse(0))),
                Math.max(0, Math.min(255, c.getOrElse(0))),
                Math.max(0, Math.min(255, w.getOrElse(0)))
              ).asSome.catchAll(_ => ZIO.none)
            else ZIO.none
          } validate
          src.fold[IO[IllegalArgumentException, Option[Source]]](ZIO.none)(s => Source.make(s).asSome.catchAll(_ => ZIO.none))
    yield Pilot(mac, timestamp, signal, state, brightness, temperature, scene, speed, color, source)
