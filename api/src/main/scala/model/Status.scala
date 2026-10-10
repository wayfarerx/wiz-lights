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
package model

/**
 * Base type for light statuses.
 */
sealed trait Status

/**
 * Definitions of the supported light statuses.
 */
object Status:

  /**
   * The light is powered but set to disabled.
   */
  case object Disabled extends Status

  /**
   * The light is set to pure white.
   *
   * @param temperature The temperature of the light.
   * @param brightness  The brightness of the light.
   */
  case class White(temperature: Temperature, brightness: Brightness) extends Status

  /**
   * The light is set to show a specific scene.
   *
   * @param scene       The scene the light is showing.
   * @param brightness  The brightness of the light.
   */
  case class Defined(scene: Scene, brightness: Brightness) extends Status

  /**
   * The light is set to a custom color.
   *
   * @param color       The color of the light.
   * @param brightness  The brightness of the light.
   */
  case class Custom(color: Color, brightness: Brightness) extends Status
