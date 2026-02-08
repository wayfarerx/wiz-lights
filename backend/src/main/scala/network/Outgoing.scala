package net.wayfarerx.wizlights.backend
package network

import protocol.Request

import cats.data.NonEmptySet

import java.net.InetAddress

/**
 * Base type for outgoing network requests.
 */
sealed trait Outgoing:

  /** The request payload to send. */
  def request: Request

/**
 * Definitions of the supported outgoing message types.
 */
object Outgoing:

  /**
   * A message that is sent to every device in the network.
   *
   * @param request The request payload to send.
   */
  case class Broadcast(request: Request) extends Outgoing

  /**
   * A message that is sent to multiple devices.
   *
   * @param addresses The addresses of the devices targeted by this message.
   * @param request   The request payload to send.
   */
  case class Multicast(addresses: NonEmptySet[InetAddress], request: Request) extends Outgoing

  /**
   * A message that is sent to a single device.
   *
   * @param address The address of the device targeted by this message.
   * @param request The request payload to send.
   */
  case class Unicast(address: InetAddress, request: Request) extends Outgoing
