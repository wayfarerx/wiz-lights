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
import zio.{Scope, UIO, URIO}

import model.*

/**
 * A service that interacts with available lights.
 */
trait Lighting:

  /** Returns the lights managed by this service. */
  def lights: UIO[Set[Light]]

  /**
   * Returns the light with the specified MAC address.
   *
   * @param macAddress The MAC address of the light to return.
   * @return The light with the specified MAC address.
   */
  def get(macAddress: MacAddress): UIO[Light]

  /**
   * Returns the lights with the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to return.
   * @return The lights with the specified MAC addresses.
   */
  def getAll(macAddresses: Iterable[MacAddress]): UIO[Set[Light]]

  /**
   * Updates the enabled state of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param enabled    True to enable the light, false to disable it.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, enabled: Boolean): UIO[Boolean]

  /**
   * Updates the enabled state of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param enabled      True to enable the lights, false to disable them.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], enabled: Boolean): UIO[Int]

  /**
   * Updates the brightness of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param brightness The brightness to set the light to.
   * @return True if a light was found and updated.
   */
  @targetName("updateBrightness")
  def update(macAddress: MacAddress, brightness: Brightness): UIO[Boolean]

  /**
   * Updates the brightness of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param brightness   The brightness to set the lights to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllBrightness")
  def updateAll(macAddresses: Iterable[MacAddress], brightness: Brightness): UIO[Int]

  /**
   * Updates the color temperature of the light at the specified MAC address.
   *
   * @param macAddress  The MAC address of the light to update.
   * @param temperature The color temperature to set the light to.
   * @return True if a light was found and updated.
   */
  @targetName("updateTemperature")
  def update(macAddress: MacAddress, temperature: Temperature): UIO[Boolean]

  /**
   * Updates the color temperature of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param temperature  The color temperature to set the lights to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllTemperature")
  def updateAll(macAddresses: Iterable[MacAddress], temperature: Temperature): UIO[Int]

  /**
   * Updates the color of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param color      The color to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, color: Color): UIO[Boolean]

  /**
   * Updates the color of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param color        The color to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], color: Color): UIO[Int]

  /**
   * Updates the scene of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param scene      The scene to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, scene: Scene): UIO[Boolean]

  /**
   * Updates the scene of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param scene        The scene to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], scene: Scene): UIO[Int]

  /**
   * Updates the dynamic scene and speed of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param scene      The dynamic scene to set the light to.
   * @param speed      The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, scene: Scene.Dynamic, speed: Speed): UIO[Boolean]

  /**
   * Updates the dynamic scene and speed of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param scene        The dynamic scene to set the lights to.
   * @param speed        The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], scene: Scene.Dynamic, speed: Speed): UIO[Int]

  /**
   * Updates the dynamic scene speed of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param speed      The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  @targetName("updateSpeed")
  def update(macAddress: MacAddress, speed: Speed): UIO[Boolean]

  /**
   * Updates the dynamic scene speed of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param speed        The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllSpeed")
  def updateAll(macAddresses: Iterable[MacAddress], speed: Speed): UIO[Int]

  /**
   * Updates the status of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param status     The status to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, status: Status): UIO[Boolean]

  /**
   * Updates the statuses of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param status       The status to set the lights to.
   * @return The number of lights were found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], status: Status): UIO[Int]

  /**
   * Updates the light at the specified MAC address using the specified update.
   *
   * @param macAddress The MAC address of the light to update.
   * @param update     The update to apply to the light.
   * @return True if a light was found and updated.
   */
  final def update(macAddress: MacAddress, update: Update): UIO[Boolean] = update match
    case Update.Enabled(enabled) => this.update(macAddress, enabled)
    case Update.Brightness(brightness) => this.update(macAddress, brightness)
    case Update.Temperature(temperature) => this.update(macAddress, temperature)
    case Update.Color(color) => this.update(macAddress, color)
    case Update.Static(scene) => this.update(macAddress, scene)
    case Update.Dynamic(scene, speed) => this.update(macAddress, scene, speed)
    case Update.Speed(speed) => this.update(macAddress, speed)
    case Update.Status(status) => this.update(macAddress, status)

  /**
   * Updates the lights at the specified MAC addresses using the specified update.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param update       The update to apply to the lights.
   * @return The number of lights found and updated.
   */
  final def updateAll(macAddresses: Iterable[MacAddress], update: Update): UIO[Int] = update match
    case Update.Enabled(enabled) => updateAll(macAddresses, enabled)
    case Update.Brightness(brightness) => updateAll(macAddresses, brightness)
    case Update.Temperature(temperature) => updateAll(macAddresses, temperature)
    case Update.Color(color) => updateAll(macAddresses, color)
    case Update.Static(scene) => updateAll(macAddresses, scene)
    case Update.Dynamic(scene, speed) => updateAll(macAddresses, scene, speed)
    case Update.Speed(speed) => updateAll(macAddresses, speed)
    case Update.Status(status) => updateAll(macAddresses, status)

  /**
   * Subscribes to events from this service.
   *
   * @return A stream of events published by this service.
   */
  def subscribe: URIO[Scope, UStream[(Light, Option[Source])]]
