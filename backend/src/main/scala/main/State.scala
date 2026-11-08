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
 * @param module       The module name of the light if known.
 * @param firmware     The firmware version of the light if known.
 * @param group        The group the light belongs to if known.
 * @param device       The device type ID of the light if known.
 * @param registration       The timestamp when the light was registered if ever.
 * @param lastSeen           The timestamp when the light was last seen if ever.
 * @param authoritativeUntil The timestamp until which optimistic updates are guarded.
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
  registration: Option[Long] = None,
  lastSeen: Option[Long] = None,
  authoritativeUntil: Option[Long] = None
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

  /**
   * Merges a set of changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The changes to merge into this device.
   * @return The merged device state.
   */
  def merge(inetAddress: InetAddress, changes: Changes): State =
    if lastSeen.fold(false)(_ > changes.timestamp) then this
    else changes match
      case registered: Changes.Registered => mergeRegistered(inetAddress, registered)
      case updates: Changes.Updates => mergeUpdates(inetAddress, updates)
      case settings: Changes.Settings => mergeSettings(inetAddress, settings)
      case pilot: Changes.Pilot =>
        if authoritativeUntil.fold(false)(_ > changes.timestamp) && !pilot.source.exists(_ != Source.Udp) then
          copy(
            inetAddress = Some(inetAddress),
            signal = Some(pilot.signal),
            lastSeen = Some(changes.timestamp)
          )
        else
          val base = if pilot.source.exists(_ != Source.Udp) then copy(authoritativeUntil = None) else this
          base.mergePilot(inetAddress, pilot)

  /**
   * Merges a registration change into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The registration changes to merge into this device.
   * @return The merged device state.
   */
  private def mergeRegistered(inetAddress: InetAddress, changes: Changes.Registered): State = {
    val timestamp = Some(changes.timestamp)
    copy(
      inetAddress = Some(inetAddress),
      registration = timestamp,
      lastSeen = timestamp
    )
  }

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
        copy(
          temperature = Some(temperature),
          scene = None,
          speed = None,
          color = None
        )
      case Update.Color(color) =>
        copy(
          temperature = None,
          scene = None,
          speed = None,
          color = Some(color)
        )
      case Update.Scene(scene) =>
        scene match
          case dynamic: Scene.Dynamic =>
            copy(
              temperature = None,
              scene = Some(dynamic),
              speed = speed.orElse(Some(Speed.Medium)),
              color = None
            )
          case static: Scene.Static =>
            copy(
              temperature = None,
              scene = Some(static),
              speed = None,
              color = None
            )
      case Update.Dynamic(scene, speed) =>
        copy(
          temperature = None,
          scene = Some(scene),
          speed = Some(speed),
          color = None
        )
      case Update.Speed(speed) =>
        scene match
          case Some(_: Scene.Dynamic) =>
            copy(speed = Some(speed))
          case _ =>
            this
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
      lastSeen = Some(changes.timestamp),
      authoritativeUntil = Some(changes.timestamp + 2000L)
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
    val newBrightness = changes.brightness.orElse(brightness).orElse(Some(Brightness.Brightest))
    val (newScene, newSpeed, newTemp, newColor) =
      changes.scene match
        case Some(dynamic: Scene.Dynamic) =>
          (Some(dynamic), changes.speed.orElse(speed).orElse(Some(Speed.Medium)), None, None)
        case Some(static: Scene.Static) =>
          (Some(static), None, None, None)
        case None =>
          changes.temperature match
            case Some(temp) =>
              (None, None, Some(temp), None)
            case None =>
              changes.color match
                case Some(clr) =>
                  (None, None, None, Some(clr))
                case None =>
                  val updatedSpeed = if scene.exists(_.isDynamic) then changes.speed.orElse(speed) else speed
                  (scene, updatedSpeed, temperature, color)
    copy(
      inetAddress = Some(inetAddress),
      signal = Some(changes.signal),
      state = Some(changes.state),
      brightness = newBrightness,
      temperature = newTemp,
      scene = newScene,
      speed = newSpeed,
      color = newColor,
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
