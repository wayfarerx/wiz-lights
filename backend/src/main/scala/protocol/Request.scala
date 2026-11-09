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

import io.circe.{Encoder, Json, JsonObject}
import io.circe.generic.semiauto.deriveEncoder

import model.*

/**
 * Base type for requests.
 */
sealed trait Request:

  /** The concrete type of this request. */
  type Self <: Request
  
  /** The method this request invokes, */
  def method: String

  /** The optional correlation ID. */
  def id: Option[Int]

  /**
   * Returns a copy of this request but with the specified ID.
   *
   * @param id THe ID to set on the returned copy.
   * @return A copy of this request but with the specified ID.
   */
  def withId(id: Option[Int]): Self

/**
 * Definitions associated with requests.
 */
object Request:
  
  /** The generic request encoder. */
  given Encoder[Request] = Encoder.instance {
    case request: GetPilotRequest => GetPilotRequest.encoder(request)
    case request: GetSystemConfigRequest => GetSystemConfigRequest.encoder(request)
    case request: RegistrationRequest => RegistrationRequest.encoder(request)
    case request: SetPilotRequest => SetPilotRequest.encoder(request)
  }

  /**
   * Wraps request encoders with the protocol envelope.
   *
   * @tparam T The type of request being encoded.
   * @param method  The method name.
   * @param encoder The encoder to wrap in an envelope.
   * @return The request encoder wrapped in the protocol envelope.
   */
  private[protocol] def encoding[T <: Request](method: String, encoder: Encoder[T]): Encoder[T] =
    val jsonMethod = Some(MethodKey -> Json.fromString(method))
    encoder.mapJson {
      _.mapObject { json =>
        JsonObject(Seq(
          json(IdKey).map(IdKey -> _),
          jsonMethod,
          Some(ParamsKey -> json.remove(IdKey).toJson.dropNullValues)
        ).flatten *)
      }.dropNullValues
    }

/**
 * The get pilot request.
 *
 * @param id The optional correlation ID.
 */
case class GetPilotRequest(id: Option[Int] = None) extends Request:

  /* The concrete type of this request. */
  override type Self = GetPilotRequest

  /* The method this request invokes, */
  override def method: String = GetPilotMethod

  /* Return a copy of this request but with the specified ID. */
  override def withId(id: Option[Int]): Self = copy(id = id)

/**
 * Definitions associated with get pilot requests.
 */
object GetPilotRequest:

  /** The get pilot request encoder. */
  given encoder: Encoder[GetPilotRequest] = Request.encoding(GetPilotMethod, deriveEncoder)

/**
 * The get system config request.
 *
 * @param id The optional correlation ID.
 */
case class GetSystemConfigRequest(id: Option[Int] = None) extends Request:

  /* The concrete type of this request. */
  override type Self = GetSystemConfigRequest

  /* The method this request invokes, */
  override def method: String = GetSystemConfigMethod

  /* Return a copy of this request but with the specified ID. */
  override def withId(id: Option[Int]): Self = copy(id = id)

/**
 * Definitions associated with get system config requests.
 */
object GetSystemConfigRequest:

  /** The get system config request encoder. */
  given encoder: Encoder[GetSystemConfigRequest] = Request.encoding(GetSystemConfigMethod, deriveEncoder)

/**
 * The registration request.
 *
 * @param phoneMac The MAC address of the device registering for updates.
 * @param phoneIp  The IP address of the device registering for updates.
 * @param register True to register for updates, false to deregister.
 * @param id       The optional correlation ID.
 */
case class RegistrationRequest(
  phoneMac: String,
  phoneIp: String,
  register: Boolean = true,
  id: Option[Int] = None
) extends Request:

  /* The concrete type of this request. */
  override type Self = RegistrationRequest

  /* The method this request invokes, */
  override def method: String = RegistrationMethod

  /* Return a copy of this request but with the specified ID. */
  override def withId(id: Option[Int]): Self = copy(id = id)

/**
 * Definitions associated with registration requests.
 */
object RegistrationRequest:

  /** The registration request encoder. */
  given encoder: Encoder[RegistrationRequest] = Request.encoding(RegistrationMethod, deriveEncoder)

/**
 * The set pilot request.
 *
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
 * @param id      The optional correlation ID.
 */
case class SetPilotRequest(
  state: Option[Boolean] = None,
  dimming: Option[Int] = None,
  sceneId: Option[Int] = None,
  speed: Option[Int] = None,
  temp: Option[Int] = None,
  r: Option[Int] = None,
  g: Option[Int] = None,
  b: Option[Int] = None,
  c: Option[Int] = None,
  w: Option[Int] = None,
  id: Option[Int] = None
) extends Request:

  /* The concrete type of this request. */
  override type Self = SetPilotRequest

  /* The method this request invokes, */
  override def method: String = SetPilotMethod

  /* Return a copy of this request but with the specified ID. */
  override def withId(id: Option[Int]): Self = copy(id = id)

/**
 * Definitions associated with set pilot requests.
 */
object SetPilotRequest:

  /** The set pilot request encoder. */
  given encoder: Encoder[SetPilotRequest] = Request.encoding(SetPilotMethod, deriveEncoder)

  /**
   * Creates a set pilot request from a domain status.
   *
   * @param status The status to set.
   * @return A set pilot request matching the status.
   */
  def from(status: Status): SetPilotRequest =
    from(status, None)

  /**
   * Creates a set pilot request from a domain status.
   *
   * @param status The status to set.
   * @param id     The optional correlation ID.
   * @return A set pilot request matching the status.
   */
  def from(status: Status, id: Option[Int]): SetPilotRequest = status match
    case Status.Disabled =>
      SetPilotRequest(state = Some(false), id = id)
    case Status.White(temp, brightness) =>
      SetPilotRequest(state = Some(true), temp = Some(temp), dimming = Some(brightness), id = id)
    case Status.Defined.Static(scene, brightness) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), dimming = Some(brightness), id = id)
    case Status.Defined.Dynamic(scene, speed, brightness) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed), dimming = Some(brightness), id = id)
    case Status.Custom(color, brightness) =>
      SetPilotRequest(
        state = Some(true),
        dimming = Some(brightness),
        r = Some(color.red),
        g = Some(color.green),
        b = Some(color.blue),
        c = Some(color.cool),
        w = Some(color.warm),
        id = id
      )

  /**
   * Creates a set pilot request from a domain update.
   *
   * @param update The update to apply.
   * @return A set pilot request representing the update.
   */
  def from(update: Update): SetPilotRequest =
    from(update, None)

  /**
   * Creates a set pilot request from a domain update.
   *
   * @param update The update to apply.
   * @param id     The optional correlation ID.
   * @return A set pilot request representing the update.
   */
  def from(update: Update, id: Option[Int]): SetPilotRequest = update match
    case Update.Enabled(enabled) =>
      SetPilotRequest(state = Some(enabled), id = id)
    case Update.Brightness(brightness) =>
      SetPilotRequest(state = Some(true), dimming = Some(brightness), id = id)
    case Update.Temperature(temperature) =>
      SetPilotRequest(state = Some(true), temp = Some(temperature), id = id)
    case Update.Color(color) =>
      SetPilotRequest(
        state = Some(true),
        r = Some(color.red),
        g = Some(color.green),
        b = Some(color.blue),
        c = Some(color.cool),
        w = Some(color.warm),
        id = id
      )
    case Update.Scene(scene) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), id = id)
    case Update.Dynamic(scene, speed) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed), id = id)
    case Update.Speed(speed) =>
      SetPilotRequest(speed = Some(speed), id = id)
    case Update.Status(status) =>
      from(status, id)
