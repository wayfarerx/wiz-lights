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
package backend
package protocol

import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, DecodingFailure}

/**
 * Base type for responses.
 */
sealed trait Response:

  /** The optional ID to use when correlating this response with a request. */
  def id: Option[Int]

/**
 * Definitions associated with responses.
 */
object Response:

  /** The index of decoders by method name. */
  private val decoders = Map[String, Decoder[? <: Response]](
    GetPilotMethod -> payloadDecoder[GetPilotResponse, GetPilotResponsePayload](GetPilotResponse.apply),
    SetPilotMethod -> payloadDecoder[SetPilotResponse, SetPilotResponsePayload](SetPilotResponse.apply)
  )

  /** General purpose response decoder. */
  given Decoder[Response] = Decoder.instance { cursor =>
    for
      method <- cursor.downField(MethodKey).as[String]
      decoder <- decoders.get(method).toRight(DecodingFailure(DecodingFailure.Reason.CustomReason(
        s"""The value "$method" is not supported as a value for the "$MethodKey" key."""
      ), cursor))
      result <- decoder(cursor)
    yield result
  }

  /**
   * Creates a decoder for a response type from a payload type.
   *
   * @tparam R The type of response being decoded.
   * @tparam P The type of result payload being decoded.
   * @param f The function that creates a response from an optional correlation ID and payload.
   * @return A decoder for a response type from a payload type.
   */
  private def payloadDecoder[R <: Response, P: Decoder](
    f: (Option[Int], Either[ErrorResponsePayload, P]) => R
  ): Decoder[R] = {
    Decoder.instance { cursor =>
      for
        id <- cursor.downField(IdKey).success.fold(Right(Option.empty[Int]))(_.as[Int].map(Some.apply))
        result <- (cursor.downField(ResultKey).success, cursor.downField(ErrorKey).success) match
          case (Some(result), None) =>
            result.as[P].map(p => f(id, Right(p)))
          case (None, Some(error)) =>
            error.as[ErrorResponsePayload].map(e => f(id, Left(e)))
          case (Some(_), Some(_)) =>
            Left(DecodingFailure(DecodingFailure.Reason.CustomReason(
              s"""Only one of the "$ResultKey" key or the "$ErrorKey" key may be present."""
            ), cursor))
          case (None, None) =>
            Left(DecodingFailure(DecodingFailure.Reason.CustomReason(
              s"""Either the "$ResultKey" key or the "$ErrorKey" key must be present."""
            ), cursor))
      yield result
    }
  }

/**
 * The get pilot response.
 *
 * @param id The optional ID to use when correlating this response with a request.
 * @param outcome Either an error payload or the get pilot payload.
 */
case class GetPilotResponse(
  id: Option[Int],
  outcome: Either[ErrorResponsePayload, GetPilotResponsePayload]
) extends Response

/**
 * The set pilot response.
 *
 * @param id The optional ID to use when correlating this response with a request.
 * @param outcome Either an error payload or the set pilot payload.
 */
case class SetPilotResponse(
  id: Option[Int],
  outcome: Either[ErrorResponsePayload, SetPilotResponsePayload]
) extends Response

/**
 * The get pilot response payload.
 *
 * @param mac     The mac address of the device.
 * @param rssi    The Signal Strength Indicator, usually between -30 (excellent) and -90 (poor)
 * @param state   The state of the device.
 * @param dimming The brightness level of the device (10 - 100).
 * @param sceneId The ID of the scene used by the device.
 * @param speed   The animated scene speed of the device (10 - 200).
 * @param temp    The color temperature of the device (2,200 - 6,500).
 * @param r       The red color component (0 - 255).
 * @param g       The green color component (0 - 255).
 * @param b       The blue color component (0 - 255).
 * @param c       The cool white component (0 - 255).
 * @param w       The warm white component (0 - 255).
 */
case class GetPilotResponsePayload(
  mac: String,
  rssi: Int,
  state: Boolean,
  dimming: Option[Int],
  sceneId: Option[Int],
  speed: Option[Int],
  temp: Option[Int],
  r: Option[Int],
  g: Option[Int],
  b: Option[Int],
  c: Option[Int],
  w: Option[Int]
)

/**
 * Definitions associated with get pilot response payloads.
 */
object GetPilotResponsePayload:

  /** The get pilot response payload decoder. */
  given Decoder[GetPilotResponsePayload] = deriveDecoder[GetPilotResponsePayload]

/**
 * The set pilot response.
 *
 * @param success True if the associated request succeeded.
 */
case class SetPilotResponsePayload(success: Boolean)

/**
 * Definitions associated with set pilot response payloads.
 */
object SetPilotResponsePayload:

  /** The set pilot response payload decoder. */
  given Decoder[SetPilotResponsePayload] = deriveDecoder[SetPilotResponsePayload]

/**
 * The error response payload.
 *
 * @param code The error code that was returned.
 * @param message The error message that was returned.
 */
case class ErrorResponsePayload(code: Option[Int], message: Option[String])

/**
 * Definitions associated with error response payloads.
 */
object ErrorResponsePayload:

  /** The error response payload decoder. */
  given Decoder[ErrorResponsePayload] = deriveDecoder[ErrorResponsePayload]
