package net.wayfarerx.wizlights
package model

/**
 * Information about a powered light.
 *
 * @param location The location of this light.
 * @param state    The state of this light.
 */
case class Light(location: Location, state: Light.State):

  /** Returns the name of this light. */
  def name: String = location.name

/**
 * Definitions associated with lights.
 */
object Light:

  /**
   * Base type for light states.
   */
  sealed trait State

  /**
   * The light is powered but set to inactive.
   */
  case object NoState extends State

  /**
   * The light is set to pure white.
   *
   * @param temperature The temperature of the light.
   * @param brightness  The brightness of the light.
   */
  case class WhiteState(temperature: Temperature, brightness: Brightness) extends State

  /**
   * The light is set to a specific color.
   *
   * @param color      The color of the light.
   * @param brightness The brightness of the light.
   */
  case class ColorState(color: Color, brightness: Brightness) extends State

  /**
   * The light is set to show a specific scene.
   *
   * @param scene      The scene the light is showing.
   * @param brightness The brightness of the light.
   */
  case class SceneState(scene: Scene, brightness: Brightness) extends State
