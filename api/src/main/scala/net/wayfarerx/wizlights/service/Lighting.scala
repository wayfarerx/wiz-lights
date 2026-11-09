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

import zio.{Scope, UIO, URIO}
import zio.stream.UStream

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
  def getAll(macAddresses: Iterable[MacAddress]): UIO[Map[MacAddress, Light]]

  /**
   * Subscribes to events from this service.
   *
   * @return A stream of events published by this service.
   */
  def subscribe: URIO[Scope, UStream[(Light, Option[Source])]]

  /**
   * Updates the enabled state of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param enabled    True to enable the light, false to disable it.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, enabled: Boolean): UIO[Unit]

  /**
   * Updates the enabled state of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param enabled      True to enable the lights, false to disable them.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], enabled: Boolean): UIO[Unit]

  /**
   * Updates the brightness of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param brightness The brightness to set the light to.
   * @return True if a light was found and updated.
   */
  @annotation.targetName("updateBrightness")
  def update(macAddress: MacAddress, brightness: Brightness): UIO[Unit]

  /**
   * Updates the brightness of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param brightness   The brightness to set the lights to.
   * @return The number of lights found and updated.
   */
  @annotation.targetName("updateAllBrightness")
  def updateAll(macAddresses: Iterable[MacAddress], brightness: Brightness): UIO[Unit]

  /**
   * Updates the color temperature of the light at the specified MAC address.
   *
   * @param macAddress  The MAC address of the light to update.
   * @param temperature The color temperature to set the light to.
   * @return True if a light was found and updated.
   */
  @annotation.targetName("updateTemperature")
  def update(macAddress: MacAddress, temperature: Temperature): UIO[Unit]

  /**
   * Updates the color temperature of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param temperature  The color temperature to set the lights to.
   * @return The number of lights found and updated.
   */
  @annotation.targetName("updateAllTemperature")
  def updateAll(macAddresses: Iterable[MacAddress], temperature: Temperature): UIO[Unit]

  /**
   * Updates the color of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param color      The color to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, color: Color): UIO[Unit]

  /**
   * Updates the color of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param color        The color to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], color: Color): UIO[Unit]

  /**
   * Updates the scene of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param scene      The scene to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, scene: Scene): UIO[Unit]

  /**
   * Updates the scene of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param scene        The scene to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], scene: Scene): UIO[Unit]

  /**
   * Updates the dynamic scene and speed of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param scene      The dynamic scene to set the light to.
   * @param speed      The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, scene: Scene.Dynamic, speed: Speed): UIO[Unit]

  /**
   * Updates the dynamic scene and speed of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param scene        The dynamic scene to set the lights to.
   * @param speed        The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], scene: Scene.Dynamic, speed: Speed): UIO[Unit]

  /**
   * Updates the dynamic scene speed of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param speed      The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  @annotation.targetName("updateSpeed")
  def update(macAddress: MacAddress, speed: Speed): UIO[Unit]

  /**
   * Updates the dynamic scene speed of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param speed        The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  @annotation.targetName("updateAllSpeed")
  def updateAll(macAddresses: Iterable[MacAddress], speed: Speed): UIO[Unit]

  /**
   * Updates the status of the light at the specified MAC address.
   *
   * @param macAddress The MAC address of the light to update.
   * @param status     The status to set the light to.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, status: Status): UIO[Unit]

  /**
   * Updates the statuses of the lights at the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param status       The status to set the lights to.
   * @return The number of lights were found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], status: Status): UIO[Unit]

  /**
   * Updates the light at the specified MAC address using the specified update.
   *
   * @param macAddress The MAC address of the light to update.
   * @param update     The update to apply to the light.
   * @return True if a light was found and updated.
   */
  def update(macAddress: MacAddress, update: Update): UIO[Unit]

  /**
   * Updates the lights at the specified MAC addresses using the specified update.
   *
   * @param macAddresses The MAC addresses of the lights to update.
   * @param update       The update to apply to the lights.
   * @return The number of lights found and updated.
   */
  def updateAll(macAddresses: Iterable[MacAddress], update: Update): UIO[Unit]
