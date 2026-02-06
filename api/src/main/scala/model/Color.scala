package net.wayfarerx.wizlights
package model

import zio.{Task, ZIO}

/**
 * A color a light can emit.
 *
 * @param red   The red color component in the range 0 - 255.
 * @param green The green color component in the range 0 - 255.
 * @param blue  The blue color component in the range 0 - 255.
 */
case class Color private(red: Int, green: Int, blue: Int)

/**
 * Factory for light colors.
 */
object Color:

  /** The black color. */
  val Black = Color(0, 0, 0)

  /** The silver color. */
  val Silver = Color(192, 192, 192)

  /** The gray color. */
  val Gray = Color(128, 128, 128)

  /** The white color. */
  val White = Color(255, 255, 255)

  /** The maroon color. */
  val Maroon = Color(128, 0, 0)

  /** The red color. */
  val Red = Color(255, 0, 0)

  /** The purple color. */
  val Purple = Color(128, 0, 128)

  /** The fuchsia color. */
  val Fuchsia = Color(255, 0, 255)

  /** The green color. */
  val Green = Color(0, 128, 0)

  /** The lime color. */
  val Lime = Color(0, 255, 0)

  /** The olive color. */
  val Olive = Color(128, 128, 0)

  /** The yellow color. */
  val Yellow = Color(255, 255, 0)

  /** The navy color. */
  val Navy = Color(0, 0, 128)

  /** The blue color. */
  val Blue = Color(0, 0, 255)

  /** The teal color. */
  val Teal = Color(0, 128, 128)

  /** The aqua color. */
  val Aqua = Color(0, 255, 255)

  /**
   * Creates a new color definition.
   *
   * @param red   The red color component in the range 0 - 255.
   * @param green The green color component in the range 0 - 255.
   * @param blue  The blue color component in the range 0 - 255.
   * @return The new color definition if all the color components are valid.
   */
  def make(red: Int, green: Int, blue: Int): Task[Color] = for
    (r, g, b) <- validated("red", red) validate validated("green", green) validate validated("blue", blue)
  yield Color(r, g, b)

  /**
   * Validates a color component.
   *
   * @param name      The name of the color component.
   * @param component The component value to validate.
   * @return The color component if it is valid.
   */
  private def validated(name: String, component: Int): Task[Int] =
    if component >= 0 && component <= 255 then ZIO.succeed(component) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid $name color component: $component. Must be between 0 and 255."
      ))
