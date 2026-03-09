package net.wayfarerx.wizlights
package model

import java.net.InetAddress

/**
 * Information about a powered light.
 *
 * @param location The location of this light.
 * @param state    The state of this light.
 */
case class Light(location: Location, state: Light.State):

  /** Returns the name of this light. */
  def name: String = location.name

  /** Returns the MAC address of this light. */
  def macAddress: String = location.macAddress

  /** Returns the IP address of this light. */
  def ipAddress: InetAddress = state.ipAddress

/**
 * Definitions associated with lights.
 */
object Light:

  /**
   * Base type for light states.
   */
  sealed trait State:

    /** The IP address of the light. */
    def ipAddress: InetAddress

  /**
   * The light is powered but set to inactive.
   *
   * @param ipAddress The IP address of the light.
   */
  case class InactiveState(ipAddress: InetAddress) extends State

  /**
   * The light is set to pure white.
   *
   * @param ipAddress   The IP address of the light.
   * @param temperature The temperature of the light.
   * @param brightness  The brightness of the light.
   */
  case class WhiteState(ipAddress: InetAddress, temperature: Temperature, brightness: Brightness) extends State

  /**
   * The light is set to a specific color.
   *
   * @param ipAddress  The IP address of the light.
   * @param color      The color of the light.
   * @param brightness The brightness of the light.
   */
  case class ColorState(ipAddress: InetAddress, color: Color, brightness: Brightness) extends State

  /**
   * The light is set to show a specific scene.
   *
   * @param ipAddress  The IP address of the light.
   * @param scene      The scene the light is showing.
   * @param brightness The brightness of the light.
   */
  case class SceneState(ipAddress: InetAddress, scene: Scene, brightness: Brightness) extends State
