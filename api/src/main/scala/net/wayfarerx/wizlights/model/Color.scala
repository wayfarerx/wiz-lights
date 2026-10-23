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

import cats.data.{NonEmptyList, ValidatedNel}
import cats.syntax.all.*

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}

import zio.{IO, ZIO}

/**
 * A color a light can emit.
 *
 * @param red   The red color component in the range 0 - 255.
 * @param green The green color component in the range 0 - 255.
 * @param blue  The blue color component in the range 0 - 255.
 * @param cool  The cool white color component in the range 0 - 255.
 * @param warm  The warm white color component in the range 0 - 255.
 */
case class Color private(
  red: Int,
  green: Int,
  blue: Int,
  cool: Int = 0,
  warm: Int = 0
)

/**
 * Factory for light colors.
 */
object Color:

  /** The circe encoder for colors. */
  given Encoder[Color] = Encoder.instance { color =>
    Json.fromFields(
      List(
        Some("red" -> Json.fromInt(color.red)),
        Some("green" -> Json.fromInt(color.green)),
        Some("blue" -> Json.fromInt(color.blue)),
        Option.when(color.cool != 0)("cool" -> Json.fromInt(color.cool)),
        Option.when(color.warm != 0)("warm" -> Json.fromInt(color.warm))
      ).flatten
    )
  }

  /** The circe decoder for colors. */
  given Decoder[Color] = new Decoder[Color]:

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Color] =
      val red = Decoder[Int].tryDecodeAccumulating(cursor.downField("red"))
      val green = Decoder[Int].tryDecodeAccumulating(cursor.downField("green"))
      val blue = Decoder[Int].tryDecodeAccumulating(cursor.downField("blue"))
      val cool = Decoder[Option[Int]].tryDecodeAccumulating(cursor.downField("cool")).map(_.getOrElse(0))
      val warm = Decoder[Option[Int]].tryDecodeAccumulating(cursor.downField("warm")).map(_.getOrElse(0))
      (red, green, blue, cool, warm).tupled.andThen { (r, g, b, c, w) =>
        from(r, g, b, c, w).left.map { errs =>
          NonEmptyList.fromListUnsafe(
            errs.map(msg => DecodingFailure(DecodingFailure.Reason.CustomReason(msg), cursor))
          )
        }.toValidated
      }

    override def apply(cursor: HCursor): Decoder.Result[Color] =
      decodeAccumulating(cursor).toEither.left.map { failures =>
        DecodingFailure(
          DecodingFailure.Reason.CustomReason(failures.map(_.message).toList.mkString(" ")),
          cursor
        )
      }

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
   * Attempts to create a new color definition.
   *
   * @param red   The red color component in the range 0 - 255.
   * @param green The green color component in the range 0 - 255.
   * @param blue  The blue color component in the range 0 - 255.
   * @param cool  The cool white color component in the range 0 - 255.
   * @param warm  The warm white color component in the range 0 - 255.
   * @return The new color definition if valid or a failure.
   */
  def from(red: Int, green: Int, blue: Int, cool: Int = 0, warm: Int = 0): Either[List[String], Color] =
    val errors = List(
      fromComponent("red", red),
      fromComponent("green", green),
      fromComponent("blue", blue),
      fromComponent("cool", cool),
      fromComponent("warm", warm)
    ).collect { case Left(err) => err }
    if errors.isEmpty then Right(Color(red, green, blue, cool, warm))
    else Left(errors)

  /**
   * Creates a new color definition.
   *
   * @param red   The red color component in the range 0 - 255.
   * @param green The green color component in the range 0 - 255.
   * @param blue  The blue color component in the range 0 - 255.
   * @param cool  The cool white color component in the range 0 - 255.
   * @param warm  The warm white color component in the range 0 - 255.
   * @return The new color definition if all the color components are valid.
   */
  def make(red: Int, green: Int, blue: Int, cool: Int = 0, warm: Int = 0): IO[IllegalArgumentException, Color] = for
    (r, g, b, c, w) <-
      makeComponent("red", red) validate
        makeComponent("green", green) validate
        makeComponent("blue", blue) validate
        makeComponent("cool", cool) validate
        makeComponent("warm", warm)
  yield Color(r, g, b, c, w)

  /**
   * Validates a color component.
   *
   * @param name      The name of the color component.
   * @param component The component value to validate.
   * @return The color component if it is valid or an error message.
   */
  private def fromComponent(name: String, component: Int): Either[String, Int] =
    if component >= 0 && component <= 255 then Right(component) else
      Left(s"Invalid $name color component: $component. Must be between 0 and 255.")

  /**
   * Validates a color component.
   *
   * @param name      The name of the color component.
   * @param component The component value to validate.
   * @return The color component if it is valid or an error message.
   */
  private def makeComponent(name: String, component: Int): IO[IllegalArgumentException, Int] =
    ZIO.fromEither(fromComponent(name, component)).mapError(IllegalArgumentException(_))
