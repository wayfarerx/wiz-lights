package net.wayfarerx.wizlights
package backend
package protocol

import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax.*
import io.circe.{Encoder, Json}

/**
 * Base type for requests.
 */
sealed trait Request

/**
 * Definitions associated with requests.
 */
object Request:

  /** The generic request encoder. */
  given Encoder[Request] = Encoder.instance {
    case request@GetPilotRequest => request.asJson
    case request: SetPilotRequest => request.asJson
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
    encoder.mapJson { json =>
      Json.obj(
        "method" -> Json.fromString(method),
        "params" -> json.dropNullValues
      )
    }

/**
 * The get pilot request.
 */
case object GetPilotRequest extends Request:

  /** The get pilot request encoder. */
  given Encoder[GetPilotRequest.type] = Request.encoding(GetPilot, deriveEncoder)

/**
 * The set pilot request.
 *
 * @param state   The state of the device.
 * @param temp    The color temperature of the device (2,200 - 6,500).
 * @param r       The red color component (0 - 255).
 * @param g       The green color component (0 - 255).
 * @param b       The blue color component (0 - 255).
 * @param sceneId The ID of the scene used by the device.
 * @param dimming The brightness level of the device (10 - 100).
 */
case class SetPilotRequest(
  state: Boolean,
  temp: Option[Int] = None,
  r: Option[Int] = None,
  g: Option[Int] = None,
  b: Option[Int] = None,
  sceneId: Option[Int] = None,
  dimming: Option[Int] = None
) extends Request

/**
 * Definitions associated with set pilot requests.
 */
object SetPilotRequest:

  /** The set pilot request encoder. */
  given Encoder[SetPilotRequest] = Request.encoding(SetPilot, deriveEncoder)
