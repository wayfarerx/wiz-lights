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

/**
 * Internal record representing the accumulated state of a light.
 *
 * @param macAddress  The MAC address of the light.
 * @param inetAddress The IP address of the light if known.
 * @param timestamp   The timestamp when the light was last seen if ever.
 * @param status      The status of the light if known.
 * @param signal      The signal strength of the light if known.
 * @param home        The home the light belongs to if known.
 * @param room        The room the light belongs to if known.
 * @param module      The module name of the light if known.
 * @param firmware    The firmware version of the light if known.
 * @param group       The group the light belongs to if known.
 * @param device      The device type ID of the light if known.
 */
private case class DeviceState(
  macAddress: MacAddress,
  inetAddress: Option[InetAddress] = None,
  timestamp: Option[Timestamp] = None,
  signal: Option[Signal] = None,
  status: Option[Status] = None,
  home: Option[Home] = None,
  room: Option[Room] = None,
  module: Option[Module] = None,
  firmware: Option[Firmware] = None,
  group: Option[Group] = None,
  device: Option[Device] = None
):

  /**
   * Returns true if there has been changes over time between this state and that state.
   *
   * @param that The state to compare this state to.
   */
  def changed(that: DeviceState): Boolean =
    copy(timestamp = None) != that.copy(timestamp = None)

  /**
   * Merges a set of changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The changes to merge into this device.
   * @return The merged device state.
   */
  def merge(inetAddress: InetAddress, changes: DeviceChanges): DeviceState = changes match
    case settings: DeviceChanges.Settings => merge(inetAddress, settings)
    case pilot: DeviceChanges.Pilot => merge(inetAddress, pilot)

  /**
   * Merges a set of settings changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The setting changes to merge into this device.
   * @return The merged device state.
   */
  private def merge(inetAddress: InetAddress, changes: DeviceChanges.Settings): DeviceState = copy(
    inetAddress = Some(inetAddress),
    timestamp = Some(changes.timestamp),
    home = changes.home.orElse(home),
    room = changes.room.orElse(room),
    module = changes.module.orElse(module),
    firmware = changes.firmware.orElse(firmware),
    group = changes.group.orElse(group),
    device = changes.device.orElse(device)
  )

  /**
   * Merges a set of pilot changes into this device.
   *
   * @param inetAddress The internet address the changes originated from.
   * @param changes     The pilot changes to merge into this device.
   * @return The merged device state.
   */
  private def merge(inetAddress: InetAddress, changes: DeviceChanges.Pilot): DeviceState =
    val merged: Status = if !changes.state then Status.Disabled else
      val brightness = changes.brightness
        .orElse(status.collect { case e: Status.Enabled => e.brightness })
        .getOrElse(Brightness.Brightest)
      changes.scene match
        case Some(dynamic: Scene.Dynamic) =>
          val speed = changes.speed
            .orElse(status.collect { case d: Status.Defined.Dynamic => d.speed })
            .getOrElse(Speed.Medium)
          Status.Defined.Dynamic(dynamic, speed, brightness)
        case Some(static: Scene.Static) =>
          Status.Defined.Static(static, brightness)
        case None =>
          changes.temperature match
            case Some(temperature) =>
              Status.White(temperature, brightness)
            case None =>
              changes.color match
                case Some(color) =>
                  Status.Custom(color, brightness)
                case None =>
                  status match
                    case Some(Status.White(temperature, _)) =>
                      Status.White(temperature, brightness)
                    case Some(Status.Defined.Static(scene, _)) =>
                      Status.Defined.Static(scene, brightness)
                    case Some(Status.Defined.Dynamic(scene, speed, _)) =>
                      Status.Defined.Dynamic(scene, changes.speed.getOrElse(speed), brightness)
                    case Some(Status.Custom(color, _)) =>
                      Status.Custom(color, brightness)
                    case Some(Status.Disabled) | None =>
                      Status.White(Temperature.Medium, brightness)
    copy(
      inetAddress = Some(inetAddress),
      timestamp = Some(changes.timestamp),
      signal = Some(changes.signal),
      status = Some(merged)
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
        device = device,
        timestamp = timestamp
      )
    }
  )
