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

import model.*
import service.*

/**
 * Internal record representing the accumulated state of a light.
 *
 * @param macAddress  The MAC address of the light.
 * @param inetAddress The IP address of the light if known.
 * @param signal      The signal strength of the light if known.
 * @param state       Whether the light is enabled if known.
 * @param brightness  The brightness of the light if known.
 * @param temperature The color temperature of the light if known.
 * @param scene       The scene of the light if known.
 * @param speed       The dynamic scene speed of the light if known.
 * @param color       The color of the light if known.
 * @param home        The home the light belongs to if known.
 * @param room        The room the light belongs to if known.
 * @param module      The module name of the light if known.
 * @param firmware    The firmware version of the light if known.
 * @param group       The group the light belongs to if known.
 * @param device      The device type ID of the light if known.
 * @param lastSeen    The timestamp when the light was last seen if ever.
 */
private case class State(
  macAddress: MacAddress,
  inetAddress: Option[InetAddress] = None,
  signal: Option[Signal] = None,
  state: Option[Boolean] = None,
  brightness: Option[Brightness] = None,
  temperature: Option[Temperature] = None,
  scene: Option[Scene] = None,
  speed: Option[Speed] = None,
  color: Option[Color] = None,
  home: Option[Home] = None,
  room: Option[Room] = None,
  module: Option[Module] = None,
  firmware: Option[Firmware] = None,
  group: Option[Group] = None,
  device: Option[Device] = None,
  lastSeen: Option[Long] = None
):

  /** Returns the status of the light if known. */
  def status: Option[Status] = state.map {
    case false => Status.Disabled
    case true =>
      val b = brightness.getOrElse(Brightness.Brightest)
      scene match
        case Some(dynamic: Scene.Dynamic) =>
          Status.Defined.Dynamic(dynamic, speed.getOrElse(Speed.Medium), b)
        case Some(static: Scene.Static) =>
          Status.Defined.Static(static, b)
        case None =>
          temperature match
            case Some(t) =>
              Status.White(t, b)
            case None =>
              color match
                case Some(c) =>
                  Status.Custom(c, b)
                case None =>
                  Status.White(Temperature.Medium, b)
  }

  /** Returns a copy of this device state with no last seen timestamp. */
  def timeless: State = copy(lastSeen = None)

  /**
   * Merges a set of changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The changes to merge into this device.
   * @return The merged device state.
   */
  def merge(inetAddress: InetAddress, changes: Changes): State = changes match
    case updates: Changes.Updates => mergeUpdates(inetAddress, updates)
    case settings: Changes.Settings => mergeSettings(inetAddress, settings)
    case pilot: Changes.Pilot => mergePilot(inetAddress, pilot)

  /**
   * Merges an update change into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The update changes to merge into this device.
   * @return The merged device state.
   */
  private def mergeUpdates(inetAddress: InetAddress, changes: Changes.Updates): State =
    val newState = changes.update match
      case Update.Enabled(enabled) =>
        copy(state = Some(enabled))
      case Update.Brightness(brightness) =>
        copy(brightness = Some(brightness))
      case Update.Temperature(temperature) =>
        copy(temperature = Some(temperature))
      case Update.Color(color) =>
        copy(color = Some(color))
      case Update.Scene(scene) =>
        copy(scene = Some(scene))
      case Update.Dynamic(scene, speed) =>
        copy(scene = Some(scene), speed = Some(speed))
      case Update.Speed(speed) =>
        copy(speed = Some(speed))
      case Update.Status(status) =>
        status match
          case Status.Disabled =>
            copy(state = Some(false))
          case Status.White(temp, b) =>
            copy(
              state = Some(true),
              brightness = Some(b),
              temperature = Some(temp),
              scene = None,
              speed = None,
              color = None
            )
          case Status.Defined.Static(scn, b) =>
            copy(
              state = Some(true),
              brightness = Some(b),
              temperature = None,
              scene = Some(scn),
              speed = None,
              color = None
            )
          case Status.Defined.Dynamic(scn, spd, b) =>
            copy(
              state = Some(true),
              brightness = Some(b),
              temperature = None,
              scene = Some(scn),
              speed = Some(spd),
              color = None
            )
          case Status.Custom(clr, b) =>
            copy(
              state = Some(true),
              brightness = Some(b),
              temperature = None,
              scene = None,
              speed = None,
              color = Some(clr)
            )
    newState.copy(
      inetAddress = Some(inetAddress),
      lastSeen = Some(changes.timestamp)
    )

  /**
   * Merges a set of settings changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The setting changes to merge into this device.
   * @return The merged device state.
   */
  private def mergeSettings(inetAddress: InetAddress, changes: Changes.Settings): State = copy(
    inetAddress = Some(inetAddress),
    home = changes.home.orElse(home),
    room = changes.room.orElse(room),
    module = changes.module.orElse(module),
    firmware = changes.firmware.orElse(firmware),
    group = changes.group.orElse(group),
    device = changes.device.orElse(device),
    lastSeen = Some(changes.timestamp)
  )

  /**
   * Merges a set of pilot changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The pilot changes to merge into this device.
   * @return The merged device state.
   */
  private def mergePilot(inetAddress: InetAddress, changes: Changes.Pilot): State =
    if !changes.state then
      copy(
        inetAddress = Some(inetAddress),
        signal = Some(changes.signal),
        state = Some(false),
        lastSeen = Some(changes.timestamp)
      )
    else
      val newBrightness = changes.brightness.orElse(brightness).orElse(Some(Brightness.Brightest))
      changes.scene match
        case Some(dynamic: Scene.Dynamic) =>
          copy(
            inetAddress = Some(inetAddress),
            signal = Some(changes.signal),
            state = Some(true),
            brightness = newBrightness,
            temperature = None,
            scene = Some(dynamic),
            speed = changes.speed.orElse(speed).orElse(Some(Speed.Medium)),
            color = None,
            lastSeen = Some(changes.timestamp)
          )
        case Some(static: Scene.Static) =>
          copy(
            inetAddress = Some(inetAddress),
            signal = Some(changes.signal),
            state = Some(true),
            brightness = newBrightness,
            temperature = None,
            scene = Some(static),
            speed = None,
            color = None,
            lastSeen = Some(changes.timestamp)
          )
        case None =>
          changes.temperature match
            case Some(temp) =>
              copy(
                inetAddress = Some(inetAddress),
                signal = Some(changes.signal),
                state = Some(true),
                brightness = newBrightness,
                temperature = Some(temp),
                scene = None,
                speed = None,
                color = None,
                lastSeen = Some(changes.timestamp)
              )
            case None =>
              changes.color match
                case Some(clr) =>
                  copy(
                    inetAddress = Some(inetAddress),
                    signal = Some(changes.signal),
                    state = Some(true),
                    brightness = newBrightness,
                    temperature = None,
                    scene = None,
                    speed = None,
                    color = Some(clr),
                    lastSeen = Some(changes.timestamp)
                  )
                case None =>
                  // Retain existing mode
                  scene match
                    case Some(dynamic: Scene.Dynamic) =>
                      copy(
                        inetAddress = Some(inetAddress),
                        signal = Some(changes.signal),
                        state = Some(true),
                        brightness = newBrightness,
                        speed = changes.speed.orElse(speed),
                        lastSeen = Some(changes.timestamp)
                      )
                    case Some(_: Scene.Static) =>
                      copy(
                        inetAddress = Some(inetAddress),
                        signal = Some(changes.signal),
                        state = Some(true),
                        brightness = newBrightness,
                        lastSeen = Some(changes.timestamp)
                      )
                    case None =>
                      if temperature.isDefined then
                        copy(
                          inetAddress = Some(inetAddress),
                          signal = Some(changes.signal),
                          state = Some(true),
                          brightness = newBrightness,
                          lastSeen = Some(changes.timestamp)
                        )
                      else if color.isDefined then
                        copy(
                          inetAddress = Some(inetAddress),
                          signal = Some(changes.signal),
                          state = Some(true),
                          brightness = newBrightness,
                          lastSeen = Some(changes.timestamp)
                        )
                      else
                        copy(
                          inetAddress = Some(inetAddress),
                          signal = Some(changes.signal),
                          state = Some(true),
                          brightness = newBrightness,
                          temperature = Some(Temperature.Medium),
                          scene = None,
                          speed = None,
                          color = None,
                          lastSeen = Some(changes.timestamp)
                        )

  /** Returns this device state as a light. */
  def toLight: Light = Light(
    macAddress,
    status,
    inetAddress.flatMap(inetAddress => IpAddress.from(inetAddress.getHostAddress).toOption).map { ipAddress =>
      Info(
        ipAddress = ipAddress,
        signal = signal,
        home = home,
        room = room,
        module = module,
        firmware = firmware,
        group = group,
        device = device
      )
    }
  )
