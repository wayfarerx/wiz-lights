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

/**
 * An event that can be applied to one or more lights.
 */
sealed trait Update

/**
 * Definitions of the supported light updates.
 */
object Update:

  /**
   * An update to the enabled state of a light.
   *
   * @param enabled True to enable the light, false to disable it.
   */
  case class Enabled(enabled: Boolean) extends Update

  /**
   * An update to the brightness of a light.
   *
   * @param brightness The brightness to set the light to.
   */
  case class Brightness(brightness: model.Brightness) extends Update

  /**
   * An update to the color temperature of a light.
   *
   * @param temperature The color temperature to set the light to.
   */
  case class Temperature(temperature: model.Temperature) extends Update

  /**
   * An update to the color of a light.
   *
   * @param color The color to set the light to.
   */
  case class Color(color: model.Color) extends Update

  /**
   * An update to the static scene and speed of a light.
   *
   * @param scene The scene to set the light to.
   * 
   */
  case class Static(scene: model.Scene.Static) extends Update

  /**
   * An update to the dynamic scene and speed of a light.
   *
   * @param scene The scene to set the light to.
   * @param speed The speed to set the dynamic scene to.
   */
  case class Dynamic(scene: model.Scene.Dynamic, speed: model.Speed) extends Update

  /**
   * An update to the dynamic scene speed of a light.
   *
   * @param speed The speed to set the dynamic scene to.
   */
  case class Speed(speed: model.Speed) extends Update

  /**
   * An update to the status of a light.
   *
   * @param status The status to set the light to.
   */
  case class Status(status: model.Status) extends Update
