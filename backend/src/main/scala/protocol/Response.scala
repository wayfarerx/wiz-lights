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
sealed trait Response

/**
 * Definitions associated with responses.
 */
object Response:

  /** The index of decoders by method name. */
  private val decoders = Map[String, Decoder[? <: Response]](
    FirstBeatMethod ->
      payloadDecoder[FirstBeatResponse, FirstBeatResponsePayload](ParamsKey, FirstBeatResponse.apply),
    GetPilotMethod ->
      payloadDecoder[GetPilotResponse, GetPilotResponsePayload](ResultKey, GetPilotResponse.apply),
    GetSystemConfigMethod ->
      payloadDecoder[GetSystemConfigResponse, GetSystemConfigResponsePayload](ResultKey, GetSystemConfigResponse.apply),
    RegistrationMethod ->
      payloadDecoder[RegistrationResponse, RegistrationResponsePayload](ResultKey, RegistrationResponse.apply),
    SetPilotMethod ->
      payloadDecoder[SetPilotResponse, SetPilotResponsePayload](ResultKey, SetPilotResponse.apply),
    SyncPilotMethod ->
      payloadDecoder[SyncPilotResponse, SyncPilotResponsePayload](ParamsKey, SyncPilotResponse.apply)
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
   * Creates a decoder for a response type from a payload type using the specified payload key.
   *
   * @tparam R The type of response being decoded.
   * @tparam P The type of result payload being decoded.
   * @param payloadKey The name of the payload key, typically "result" or "params".
   * @param f The function that creates a response from an optional correlation ID and payload.
   * @return A decoder for a response type from a payload type.
   */
  private[protocol] def payloadDecoder[R <: Response, P: Decoder](
    payloadKey: String,
    f: Either[ErrorResponsePayload, P] => R
  ): Decoder[R] =
    Decoder.instance { cursor =>
      for
        result <- (cursor.downField(payloadKey).success, cursor.downField(ErrorKey).success) match
          case (Some(result), None) =>
            result.as[P].map(p => f(Right(p)))
          case (None, Some(error)) =>
            error.as[ErrorResponsePayload].map(e => f(Left(e)))
          case (Some(_), Some(_)) =>
            Left(DecodingFailure(DecodingFailure.Reason.CustomReason(
              s"""Only one of the "$payloadKey" key or the "$ErrorKey" key may be present."""
            ), cursor))
          case (None, None) =>
            Left(DecodingFailure(DecodingFailure.Reason.CustomReason(
              s"""Either the "$payloadKey" key or the "$ErrorKey" key must be present."""
            ), cursor))
      yield result
    }

  /**
   * Creates a decoder for a response type from a payload type using the default "result" payload key.
   *
   * @tparam R The type of response being decoded.
   * @tparam P The type of result payload being decoded.
   * @param f The function that creates a response from an optional correlation ID and payload.
   * @return A decoder for a response type from a payload type.
   */
  private[protocol] def payloadDecoder[R <: Response, P: Decoder](
    f: Either[ErrorResponsePayload, P] => R
  ): Decoder[R] =
    payloadDecoder(ResultKey, f)

/**
 * The first beat response.
 *
 * @param payload Either an error payload or the first beat payload.
 */
case class FirstBeatResponse(
  payload: Either[ErrorResponsePayload, FirstBeatResponsePayload]
) extends Response

/**
 * Definitions associated with first beat responses.
 */
object FirstBeatResponse:

  /** The first beat response decoder. */
  given Decoder[FirstBeatResponse] = Response.payloadDecoder(ParamsKey, FirstBeatResponse.apply)

/**
 * The get pilot response.
 *
 * @param payload Either an error payload or the get pilot payload.
 */
case class GetPilotResponse(
  payload: Either[ErrorResponsePayload, GetPilotResponsePayload]
) extends Response

/**
 * Definitions associated with get pilot responses.
 */
object GetPilotResponse:

  /** The get pilot response decoder. */
  given Decoder[GetPilotResponse] = Response.payloadDecoder(ResultKey, GetPilotResponse.apply)

/**
 * The get system config response.
 *
 * @param payload Either an error payload or the get system config payload.
 */
case class GetSystemConfigResponse(
  payload: Either[ErrorResponsePayload, GetSystemConfigResponsePayload]
) extends Response

/**
 * Definitions associated with get system config responses.
 */
object GetSystemConfigResponse:

  /** The get system config response decoder. */
  given Decoder[GetSystemConfigResponse] = Response.payloadDecoder(ResultKey, GetSystemConfigResponse.apply)

/**
 * The registration response.
 *
 * @param payload Either an error payload or the registration payload.
 */
case class RegistrationResponse(
  payload: Either[ErrorResponsePayload, RegistrationResponsePayload]
) extends Response

/**
 * Definitions associated with registration responses.
 */
object RegistrationResponse:

  /** The registration response decoder. */
  given Decoder[RegistrationResponse] = Response.payloadDecoder(ResultKey, RegistrationResponse.apply)

/**
 * The set pilot response.
 *
 * @param payload Either an error payload or the set pilot payload.
 */
case class SetPilotResponse(
  payload: Either[ErrorResponsePayload, SetPilotResponsePayload]
) extends Response

/**
 * Definitions associated with set pilot responses.
 */
object SetPilotResponse:

  /** The set pilot response decoder. */
  given Decoder[SetPilotResponse] = Response.payloadDecoder(ResultKey, SetPilotResponse.apply)

/**
 * The sync pilot response.
 *
 * @param payload Either an error payload or the sync pilot payload.
 */
case class SyncPilotResponse(
  payload: Either[ErrorResponsePayload, SyncPilotResponsePayload]
) extends Response

/**
 * Definitions associated with sync pilot responses.
 */
object SyncPilotResponse:

  /** The sync pilot response decoder. */
  given Decoder[SyncPilotResponse] = Response.payloadDecoder(ParamsKey, SyncPilotResponse.apply)

/**
 * The first beat response payload.
 *
 * @param mac         The mac address of the device.
 * @param homeId      The ID of the home the device belongs to.
 * @param roomId      The ID of the room the device belongs to.
 * @param rgn         The region of the device.
 * @param moduleName  The module name of the device.
 * @param fwVersion   The firmware version of the device.
 * @param groupId     The ID of the group the device belongs to.
 * @param typeId      The type ID of the device.
 * @param homeLock    Whether the home configuration is locked.
 * @param pairingLock Whether pairing is locked.
 * @param ping        The ping indicator.
 */
case class FirstBeatResponsePayload(
  mac: String,
  homeId: Option[Int] = None,
  roomId: Option[Int] = None,
  rgn: Option[String] = None,
  moduleName: Option[String] = None,
  fwVersion: Option[String] = None,
  groupId: Option[Int] = None,
  typeId: Option[Int] = None,
  homeLock: Option[Boolean] = None,
  pairingLock: Option[Boolean] = None,
  ping: Option[Int] = None
)

/**
 * Definitions associated with first beat response payloads.
 */
object FirstBeatResponsePayload:

  /** The first beat response payload decoder. */
  given Decoder[FirstBeatResponsePayload] = deriveDecoder[FirstBeatResponsePayload]

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
 * The get system config response payload.
 *
 * @param mac         The mac address of the device.
 * @param homeId      The ID of the home the device belongs to.
 * @param roomId      The ID of the room the device belongs to.
 * @param rgn         The region of the device.
 * @param moduleName  The module name of the device.
 * @param fwVersion   The firmware version of the device.
 * @param groupId     The ID of the group the device belongs to.
 * @param typeId      The type ID of the device.
 * @param homeLock    Whether the home configuration is locked.
 * @param pairingLock Whether pairing is locked.
 * @param ping        The ping indicator.
 */
case class GetSystemConfigResponsePayload(
  mac: String,
  homeId: Option[Int] = None,
  roomId: Option[Int] = None,
  rgn: Option[String] = None,
  moduleName: Option[String] = None,
  fwVersion: Option[String] = None,
  groupId: Option[Int] = None,
  typeId: Option[Int] = None,
  homeLock: Option[Boolean] = None,
  pairingLock: Option[Boolean] = None,
  ping: Option[Int] = None
)

/**
 * Definitions associated with get system config response payloads.
 */
object GetSystemConfigResponsePayload:

  /** The get system config response payload decoder. */
  given Decoder[GetSystemConfigResponsePayload] = deriveDecoder[GetSystemConfigResponsePayload]

/**
 * The registration response payload.
 *
 * @param success True if the associated request succeeded.
 */
case class RegistrationResponsePayload(success: Boolean)

/**
 * Definitions associated with registration response payloads.
 */
object RegistrationResponsePayload:

  /** The registration response payload decoder. */
  given Decoder[RegistrationResponsePayload] = deriveDecoder[RegistrationResponsePayload]

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
 * The sync pilot response payload.
 *
 * @param mac     The mac address of the device.
 * @param rssi    The Signal Strength Indicator, usually between -30 (excellent) and -90 (poor).
 * @param state   The state of the device.
 * @param src     The source of the update.
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
case class SyncPilotResponsePayload(
  mac: String,
  rssi: Int,
  state: Boolean,
  src: Option[String] = None,
  dimming: Option[Int] = None,
  sceneId: Option[Int] = None,
  speed: Option[Int] = None,
  temp: Option[Int] = None,
  r: Option[Int] = None,
  g: Option[Int] = None,
  b: Option[Int] = None,
  c: Option[Int] = None,
  w: Option[Int] = None
)

/**
 * Definitions associated with sync pilot response payloads.
 */
object SyncPilotResponsePayload:

  /** The sync pilot response payload decoder. */
  given Decoder[SyncPilotResponsePayload] = deriveDecoder[SyncPilotResponsePayload]

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
