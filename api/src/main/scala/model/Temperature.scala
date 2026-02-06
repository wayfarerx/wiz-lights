package net.wayfarerx.wizlights
package model

import zio.{Task, ZIO}

/**
 * A temperature value for white lights.
 *
 * @param value The temperature value in kelvin of the light in the range 2,200 - 6,500.
 */
case class Temperature private(value: Int)

/**
 * Factory for white light temperature values.
 */
object Temperature:

  /** The warmest possible temperature. */
  val Warmest = Temperature(2200)

  /** The coolest possible temperature. */
  val Coolest = Temperature(6500)

  /** The midpoint between the warmest and coolest temperatures. */
  val Medium = Temperature((Coolest.value - Warmest.value) / 2 + Warmest.value)

  /**
   * Creates a new white light temperature definition.
   *
   * @param value The temperature value in kelvin of the light in the range 2,200 - 6,500.
   * @return The new white light temperature definition if the value is valid.
   */
  def make(value: Int): Task[Temperature] =
    if value >= Warmest.value && value <= Coolest.value then ZIO.succeed(Temperature(value)) else
      ZIO.fail(IllegalArgumentException(
        s"Invalid temperature value: $value. Must be between ${Warmest.value} and ${Coolest.value}."
      ))
