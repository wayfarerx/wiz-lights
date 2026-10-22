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

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor, Json}
import io.circe.syntax.*

/**
 * Base type for light statuses.
 */
sealed trait Status

/**
 * Definitions of the supported light statuses.
 */
object Status:

  /** The circe encoder for light statuses. */
  given Encoder[Status] = Encoder.instance {
    case Disabled => Disabled.asJson
    case enabled: Enabled => enabled.asJson
  }

  /** The circe decoder for light statuses. */
  given Decoder[Status] = accumulating(new Decoder[Status]:

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Status] =
      cursor.value.asObject match
        case Some(obj) if obj.isEmpty =>
          Validated.valid(Disabled)
        case Some(_) =>
          Decoder[Enabled].decodeAccumulating(cursor)
        case None =>
          Validated.invalidNel(DecodingFailure(
            DecodingFailure.Reason.CustomReason("Expected a JSON object for Status."),
            cursor
          ))

    override def apply(cursor: HCursor): Decoder.Result[Status] =
      decodeAccumulating(cursor).toEither.left.map(_.head)

  )

  /**
   * Helper that creates an error-accumulating decoder that also joins all error messages on apply.
   *
   * @tparam T The type being decoded.
   * @param decoder The underlying decoder.
   * @return The error-accumulating decoder.
   */
  private def accumulating[T](decoder: Decoder[T]): Decoder[T] = new Decoder[T]:

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, T] =
      decoder.decodeAccumulating(cursor)

    override def apply(cursor: HCursor): Decoder.Result[T] =
      decoder.decodeAccumulating(cursor).toEither.left.map { failures =>
        DecodingFailure(
          DecodingFailure.Reason.CustomReason(failures.map(_.message).toList.mkString(" ")),
          cursor
        )
      }

  /**
   * The light is disabled.
   */
  case object Disabled extends Status:

    /** The circe encoder for disabled status. */
    given Encoder[Disabled.type] = Encoder.instance(_ => Json.obj())

    /** The circe decoder for disabled status. */
    given Decoder[Disabled.type] = Decoder.instance { cursor =>
      if cursor.value.asObject.exists(_.isEmpty) then Right(Disabled)
      else Left(DecodingFailure(
        DecodingFailure.Reason.CustomReason("Expected an empty object for Disabled."),
        cursor
      ))
    }

  /**
   * The light is enabled.
   */
  sealed trait Enabled extends Status:

    /** The brightness of the light. */
    def brightness: Brightness

  /**
   * Definitions associated with enabled light statuses.
   */
  object Enabled:

    /** The circe encoder for enabled light statuses. */
    given Encoder[Enabled] = Encoder.instance {
      case white: White => white.asJson
      case defined: Defined => defined.asJson
      case custom: Custom => custom.asJson
    }

    /** The circe decoder for enabled light statuses. */
    given Decoder[Enabled] = accumulating(new Decoder[Enabled]:

      override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Enabled] =
        if cursor.downField("temperature").focus.isDefined then
          Decoder[White].decodeAccumulating(cursor)
        else if cursor.downField("color").focus.isDefined then
          Decoder[Custom].decodeAccumulating(cursor)
        else if cursor.downField("scene").focus.isDefined then
          Decoder[Defined].decodeAccumulating(cursor)
        else
          Validated.invalidNel(DecodingFailure(
            DecodingFailure.Reason.CustomReason(
              "Enabled status must specify one of: 'temperature', 'color', or 'scene'."
            ),
            cursor
          ))

      override def apply(cursor: HCursor): Decoder.Result[Enabled] =
        decodeAccumulating(cursor).toEither.left.map(_.head)

    )

  /**
   * The light is set to pure white.
   *
   * @param temperature The temperature of the light.
   * @param brightness  The brightness of the light.
   */
  case class White(
    temperature: Temperature,
    brightness: Brightness
  ) extends Enabled

  /**
   * Definitions associated with white statuses.
   */
  object White:

    /** The circe encoder for white statuses. */
    given Encoder[White] =
      Encoder.forProduct2("temperature", "brightness")((w: White) => (w.temperature, w.brightness))

    /** The circe decoder for white statuses. */
    given Decoder[White] =
      accumulating(Decoder.forProduct2("temperature", "brightness")(White.apply))

  /**
   * The light is set to show a specific scene.
   */
  sealed trait Defined extends Enabled:

    /** The scene the light is showing. */
    def scene: Scene

  /**
   * The available implementations of defined statuses.
   */
  object Defined:

    /** The circe encoder for defined statuses. */
    given Encoder[Defined] = Encoder.instance {
      case static: Static => static.asJson
      case dynamic: Dynamic => dynamic.asJson
    }

    /** The circe decoder for defined statuses. */
    given Decoder[Defined] = accumulating(new Decoder[Defined]:

      override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Defined] =
        if cursor.downField("speed").focus.isDefined then
          Decoder[Dynamic].decodeAccumulating(cursor)
        else
          Decoder[Static].decodeAccumulating(cursor)

      override def apply(cursor: HCursor): Decoder.Result[Defined] =
        decodeAccumulating(cursor).toEither.left.map(_.head)

    )

    /**
     * The light is set to a static scene.
     *
     * @param scene       The static scene the light is set to.
     * @param brightness  The brightness of the light.
     */
    case class Static(
      scene: Scene.Static,
      brightness: Brightness
    ) extends Defined

    /**
     * Definitions associated with static defined statuses.
     */
    object Static:

      /** The circe encoder for static defined statuses. */
      given Encoder[Static] =
        Encoder.forProduct2("scene", "brightness")((s: Static) => (s.scene, s.brightness))

      /** The circe decoder for static defined statuses. */
      given Decoder[Static] =
        accumulating(Decoder.forProduct2("scene", "brightness")(Static.apply))

    /**
     * The light is set to a dynamic scene.
     *
     * @param scene       The dynamic scene the light is set to.
     * @param speed       The speed the dynamic scene is set to.
     * @param brightness  The brightness of the light.
     */
    case class Dynamic(
      scene: Scene.Dynamic,
      speed: Speed,
      brightness: Brightness
    ) extends Defined

    /**
     * Definitions associated with dynamic defined statuses.
     */
    object Dynamic:

      /** The circe encoder for dynamic defined statuses. */
      given Encoder[Dynamic] =
        Encoder.forProduct3("scene", "speed", "brightness")((d: Dynamic) => (d.scene, d.speed, d.brightness))

      /** The circe decoder for dynamic defined statuses. */
      given Decoder[Dynamic] =
        accumulating(Decoder.forProduct3("scene", "speed", "brightness")(Dynamic.apply))

  /**
   * The light is set to a custom color.
   *
   * @param color       The color of the light.
   * @param brightness  The brightness of the light.
   */
  case class Custom(
    color: Color,
    brightness: Brightness
  ) extends Enabled

  /**
   * Definitions associated with custom statuses.
   */
  object Custom:

    /** The circe encoder for custom statuses. */
    given Encoder[Custom] =
      Encoder.forProduct2("color", "brightness")((c: Custom) => (c.color, c.brightness))

    /** The circe decoder for custom statuses. */
    given Decoder[Custom] =
      accumulating(Decoder.forProduct2("color", "brightness")(Custom.apply))
