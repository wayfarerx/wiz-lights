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

import cats.data.{Validated, ValidatedNel}

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}
import io.circe.syntax.*

/**
 * An event that can be applied to one or more lights.
 */
sealed trait Update

/**
 * Definitions of the supported light updates.
 */
object Update:

  /** The circe encoder for light updates. */
  given Encoder[Update] = Encoder.instance {
    case enabled: Enabled => enabled.asJson
    case brightness: Brightness => brightness.asJson
    case temperature: Temperature => temperature.asJson
    case color: Color => color.asJson
    case dynamic: Dynamic => dynamic.asJson
    case scene: Scene => scene.asJson
    case speed: Speed => speed.asJson
    case status: Status => status.asJson
  }

  /** The circe decoder for light updates. */
  given Decoder[Update] = accumulate { cursor =>
    cursor.value.asObject match
      case Some(_) =>
        if cursor.downField("enabled").focus.isDefined then
          Decoder[Enabled].decodeAccumulating(cursor)
        else if cursor.downField("brightness").focus.isDefined then
          Decoder[Brightness].decodeAccumulating(cursor)
        else if cursor.downField("temperature").focus.isDefined then
          Decoder[Temperature].decodeAccumulating(cursor)
        else if cursor.downField("color").focus.isDefined then
          Decoder[Color].decodeAccumulating(cursor)
        else if cursor.downField("status").focus.isDefined then
          Decoder[Status].decodeAccumulating(cursor)
        else if cursor.downField("scene").focus.isDefined && cursor.downField("speed").focus.isDefined then
          Decoder[Dynamic].decodeAccumulating(cursor)
        else if cursor.downField("scene").focus.isDefined then
          Decoder[Scene].decodeAccumulating(cursor)
        else if cursor.downField("speed").focus.isDefined then
          Decoder[Speed].decodeAccumulating(cursor)
        else
          Validated.invalidNel(DecodingFailure(
            DecodingFailure.Reason.CustomReason(
              "Update must specify one of: 'enabled', 'brightness', 'temperature', 'color', 'status', 'scene', or 'speed'."
            ),
            cursor
          ))
      case None =>
        Validated.invalidNel(DecodingFailure(
          DecodingFailure.Reason.CustomReason("Expected a JSON object for Update."),
          cursor
        ))
  }

  /**
   * Helper that creates an error-accumulating decoder that also joins all error messages on apply.
   *
   * @tparam T The type being decoded.
   * @param decode The decoding function.
   * @return The error-accumulating decoder.
   */
  private def accumulate[T](decode: HCursor => ValidatedNel[DecodingFailure, T]): Decoder[T] = new Decoder[T]:

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, T] =
      decode(cursor)

    override def apply(cursor: HCursor): Decoder.Result[T] =
      decode(cursor).toEither.left.map { failures =>
        DecodingFailure(
          DecodingFailure.Reason.CustomReason(failures.map(_.message).toList.mkString(" ")),
          cursor
        )
      }

  /**
   * Helper that adapts an existing decoder to join error messages on apply.
   *
   * @tparam T The type being decoded.
   * @param decoder The underlying decoder.
   * @return The error-accumulating decoder.
   */
  private def accumulating[T](decoder: Decoder[T]): Decoder[T] =
    accumulate(decoder.decodeAccumulating)

  /**
   * An update to the enabled state of a light.
   *
   * @param enabled True to enable the light, false to disable it.
   */
  case class Enabled(enabled: Boolean) extends Update

  /**
   * Definitions associated with enabled updates.
   */
  object Enabled:

    /** The circe encoder for enabled updates. */
    given Encoder[Enabled] =
      Encoder.forProduct1("enabled")((e: Enabled) => e.enabled)

    /** The circe decoder for enabled updates. */
    given Decoder[Enabled] =
      accumulating(Decoder.forProduct1("enabled")(Enabled.apply))

  /**
   * An update to the brightness of a light.
   *
   * @param brightness The brightness to set the light to.
   */
  case class Brightness(brightness: model.Brightness) extends Update

  /**
   * Definitions associated with brightness updates.
   */
  object Brightness:

    /** The circe encoder for brightness updates. */
    given Encoder[Brightness] =
      Encoder.forProduct1("brightness")((b: Brightness) => b.brightness)

    /** The circe decoder for brightness updates. */
    given Decoder[Brightness] =
      accumulating(Decoder.forProduct1("brightness")(Brightness.apply))

  /**
   * An update to the color temperature of a light.
   *
   * @param temperature The color temperature to set the light to.
   */
  case class Temperature(temperature: model.Temperature) extends Update

  /**
   * Definitions associated with temperature updates.
   */
  object Temperature:

    /** The circe encoder for temperature updates. */
    given Encoder[Temperature] =
      Encoder.forProduct1("temperature")((t: Temperature) => t.temperature)

    /** The circe decoder for temperature updates. */
    given Decoder[Temperature] =
      accumulating(Decoder.forProduct1("temperature")(Temperature.apply))

  /**
   * An update to the color of a light.
   *
   * @param color The color to set the light to.
   */
  case class Color(color: model.Color) extends Update

  /**
   * Definitions associated with color updates.
   */
  object Color:

    /** The circe encoder for color updates. */
    given Encoder[Color] =
      Encoder.forProduct1("color")((c: Color) => c.color)

    /** The circe decoder for color updates. */
    given Decoder[Color] =
      accumulating(Decoder.forProduct1("color")(Color.apply))

  /**
   * An update to the scene of a light.
   *
   * @param scene The scene to set the light to.
   */
  case class Scene(scene: model.Scene) extends Update

  /**
   * Definitions associated with scene updates.
   */
  object Scene:

    /** The circe encoder for scene updates. */
    given Encoder[Scene] =
      Encoder.forProduct1("scene")((s: Scene) => s.scene)

    /** The circe decoder for scene updates. */
    given Decoder[Scene] =
      accumulating(Decoder.forProduct1("scene")(Scene.apply))

  /**
   * An update to the dynamic scene and speed of a light.
   *
   * @param scene The scene to set the light to.
   * @param speed The speed to set the dynamic scene to.
   */
  case class Dynamic(scene: model.Scene.Dynamic, speed: model.Speed) extends Update

  /**
   * Definitions associated with dynamic scene updates.
   */
  object Dynamic:

    /** The circe encoder for dynamic scene updates. */
    given Encoder[Dynamic] =
      Encoder.forProduct2("scene", "speed")((d: Dynamic) => (d.scene, d.speed))

    /** The circe decoder for dynamic scene updates. */
    given Decoder[Dynamic] =
      accumulating(Decoder.forProduct2("scene", "speed")(Dynamic.apply))

  /**
   * An update to the dynamic scene speed of a light.
   *
   * @param speed The speed to set the dynamic scene to.
   */
  case class Speed(speed: model.Speed) extends Update

  /**
   * Definitions associated with speed updates.
   */
  object Speed:

    /** The circe encoder for dynamic scene speed updates. */
    given Encoder[Speed] =
      Encoder.forProduct1("speed")((s: Speed) => s.speed)

    /** The circe decoder for dynamic scene speed updates. */
    given Decoder[Speed] =
      accumulating(Decoder.forProduct1("speed")(Speed.apply))

  /**
   * An update to the status of a light.
   *
   * @param status The status to set the light to.
   */
  case class Status(status: model.Status) extends Update

  /**
   * Definitions associated with status updates.
   */
  object Status:

    /** The circe encoder for status updates. */
    given Encoder[Status] =
      Encoder.forProduct1("status")((s: Status) => s.status)

    /** The circe decoder for status updates. */
    given Decoder[Status] =
      accumulating(Decoder.forProduct1("status")(Status.apply))
