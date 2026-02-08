package net.wayfarerx.wizlights
package backend
package protocol

import io.circe.generic.semiauto.deriveDecoder
import io.circe.{Decoder, DecodingFailure}

/**
 * Base type for responses.
 */
sealed trait Response:

  /** True if the associated request succeeded. */
  def success: Boolean

/**
 * Definitions associated with responses.
 */
object Response:

  /** The index of decoders by method name. */
  private val decoders = Map[String, Decoder[? <: Response]](
    GetPilot -> deriveDecoder[GetPilotResponse],
    SetPilot -> deriveDecoder[SetPilotResponse]
  )

  /** General purpose response decoder. */
  given Decoder[Response] = Decoder.instance { cursor =>
    val methodCursor = cursor.downField("method")
    val resultCursor = cursor.downField("result")
    for
      method <- methodCursor.as[String]
      decoder <- decoders.get(method).toRight(DecodingFailure(DecodingFailure.Reason.CustomReason(
        s"""The value "$method" is not supported in the "method" field."""
      ), methodCursor))
      result <- resultCursor.success.toRight(DecodingFailure(DecodingFailure.Reason.CustomReason(
        """The "result" field is not present."""
      ), resultCursor))
      value <- decoder(result)
    yield value
  }

/**
 * The get pilot response.
 *
 * @param mac     The mac address of the device.
 * @param state   The state of the device.
 * @param temp    The color temperature of the device (1000 - 10000).
 * @param r       The red color component (0 - 255).
 * @param g       The green color component (0 - 255).
 * @param b       The blue color component (0 - 255).
 * @param sceneId The ID of the scene used by the device.
 * @param dimming The brightness level of the device (10 - 100).
 */
case class GetPilotResponse(
  mac: String,
  state: Boolean,
  temp: Option[Int],
  r: Option[Int],
  g: Option[Int],
  b: Option[Int],
  sceneId: Option[Int],
  dimming: Option[Int]
) extends Response:

  /* Always true. */
  override def success: Boolean = true

/**
 * The set pilot response.
 *
 * @param success True if the associated request succeeded.
 */
case class SetPilotResponse(success: Boolean) extends Response
