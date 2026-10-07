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
