package net.wayfarerx.wizlights.backend
package network

import cats.data.NonEmptySet
import io.circe.{Encoder, Json}

import java.net.InetAddress

/**
 * Base type for network messages.
 */
sealed trait Message:

  /** The JSON payload in this message. */
  def json: Json

/**
 * Definitions of the supported message types.
 */
object Message:

  /**
   * An incoming network message.
   *
   *
   * @param address The address of the device that sent this message.
   * @param json    The JSON payload in this message.
   */
  case class Incoming(address: InetAddress, json: Json) extends Message

  /**
   * Base type for outgoing network messages.
   */
  sealed trait Outgoing extends Message

  /**
   * Definitions of the supported outgoing message types.
   */
  object Outgoing:

    /**
     * A message that is sent to every device in the network.
     *
     * @param json The JSON payload in this message.
     */
    case class Broadcast(json: Json) extends Outgoing

    /**
     * A message that is sent to multiple devices.
     *
     * @param addresses The addresses of the devices targeted by this message.
     * @param json      The JSON payload in this message.
     */
    case class Multicast(addresses: NonEmptySet[InetAddress], json: Json) extends Outgoing

    /**
     * A message that is sent to a single device.
     *
     * @param address The address of the device targeted by this message.
     * @param json    The JSON payload in this message.
     */
    case class Unicast(address: InetAddress, json: Json) extends Outgoing
