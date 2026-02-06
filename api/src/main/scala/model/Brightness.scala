package net.wayfarerx.wizlights
package model

import zio.{Task, ZIO}

/**
 * A brightness value for lights.
 *
 * @param value The brightness value of the light in the range 10 - 100.
 */
case class Brightness private(value: Int)

/**
 * Factory for light brightness values.
 */
object Brightness:

  /** The dimmest possible brightness. */
  val Dimmest = Brightness(10)

  /** The brightest possible brightness. */
  val Brightest = Brightness(100)

  /** The midpoint between the dimmest and brightest brightness values. */
  val Medium = Brightness((Brightest.value - Dimmest.value) / 2 + Dimmest.value)

  /**
   * Creates a new light brightness definition.
   *
   * @param value The brightness value of the light in the range 10 - 100.
   * @return The new light brightness definition if the value is valid.
   */
  def make(value: Int): Task[Brightness] =
    if value >= Dimmest.value && value <= Brightest.value then ZIO.succeed(Brightness(value)) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid brightness value: $value. Must be between ${Dimmest.value} and ${Brightest.value}."
      ))
