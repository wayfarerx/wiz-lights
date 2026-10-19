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

import model.{Address, Brightness, Color, Light, Scene, Source, Speed, Status, Temperature}

/**
 * A service that interacts with available lights.
 */
trait Lighting:

  /** Returns the lights managed by this service. */
  def lights: UIO[Set[Light]]

  /**
   * Returns the light with the specified address.
   *
   * @param address The address of the light to return.
   * @return The light with the specified address.
   */
  def get(address: Address): UIO[Light]

  /**
   * Returns the lights with the specified addresses.
   *\
   * @param addresses The addresses of the lights to return.
   * @return The lights with the specified addresses.
   */
  def getAll(addresses: Iterable[Address]): UIO[Set[Light]]

  /**
   * Updates the enabled state of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param enabled True to enable the light, false to disable it.
   * @return True if a light was found and updated.
   */
  def update(address: Address, enabled: Boolean): UIO[Boolean]

  /**
   * Updates the enabled state of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param enabled   True to enable the lights, false to disable them.
   * @return The number of lights found and updated.
   */
  def updateAll(addresses: Iterable[Address], enabled: Boolean): UIO[Int]

  /**
   * Updates the brightness of the light at the specified address.
   *
   * @param address    The address of the light to update.
   * @param brightness The brightness to set the light to.
   * @return True if a light was found and updated.
   */
  @targetName("updateBrightness")
  def update(address: Address, brightness: Brightness): UIO[Boolean]

  /**
   * Updates the brightness of the lights at the specified addresses.
   *
   * @param addresses  The addresses of the lights to update.
   * @param brightness The brightness to set the lights to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllBrightness")
  def updateAll(addresses: Iterable[Address], brightness: Brightness): UIO[Int]

  /**
   * Updates the color temperature of the light at the specified address.
   *
   * @param address     The address of the light to update.
   * @param temperature The color temperature to set the light to.
   * @return True if a light was found and updated.
   */
  @targetName("updateTemperature")
  def update(address: Address, temperature: Temperature): UIO[Boolean]

  /**
   * Updates the color temperature of the lights at the specified addresses.
   *
   * @param addresses   The addresses of the lights to update.
   * @param temperature The color temperature to set the lights to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllTemperature")
  def updateAll(addresses: Iterable[Address], temperature: Temperature): UIO[Int]

  /**
   * Updates the color of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param color   The color to set the light to.
   * @return True if a light was found and updated.
   */
  def update(address: Address, color: Color): UIO[Boolean]

  /**
   * Updates the color of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param color     The color to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(addresses: Iterable[Address], color: Color): UIO[Int]

  /**
   * Updates the scene of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param scene   The scene to set the light to.
   * @return True if a light was found and updated.
   */
  def update(address: Address, scene: Scene): UIO[Boolean]

  /**
   * Updates the scene of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param scene     The scene to set the lights to.
   * @return The number of lights found and updated.
   */
  def updateAll(addresses: Iterable[Address], scene: Scene): UIO[Int]

  /**
   * Updates the dynamic scene and speed of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param scene   The dynamic scene to set the light to.
   * @param speed   The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  def update(address: Address, scene: Scene.Dynamic, speed: Speed): UIO[Boolean]

  /**
   * Updates the dynamic scene and speed of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param scene     The dynamic scene to set the lights to.
   * @param speed     The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  def updateAll(addresses: Iterable[Address], scene: Scene.Dynamic, speed: Speed): UIO[Int]

  /**
   * Updates the dynamic scene speed of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param speed   The speed to set the dynamic scene to.
   * @return True if a light was found and updated.
   */
  @targetName("updateSpeed")
  def update(address: Address, speed: Speed): UIO[Boolean]

  /**
   * Updates the dynamic scene speed of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param speed     The speed to set the dynamic scene to.
   * @return The number of lights found and updated.
   */
  @targetName("updateAllSpeed")
  def updateAll(addresses: Iterable[Address], speed: Speed): UIO[Int]

  /**
   * Updates the state of the light at the specified address.
   *
   * @param address The address of the light to update.
   * @param status  The status to set the light to.
   * @return True if a light was found and updated.
   */
  def update(address: Address, status: Status): UIO[Boolean]

  /**
   * Updates the status of the lights at the specified addresses.
   *
   * @param addresses The addresses of the lights to update.
   * @param status    The status to set the lights to.
   * @return The number of lights were found and updated.
   */
  def updateAll(addresses: Iterable[Address], status: Status): UIO[Int]

  /**
   * Subscribes to events from this service.
   *
   * @return A stream of events published by this service.
   */
  def subscribe: URIO[Scope, UStream[(Light, Option[Source])]]
