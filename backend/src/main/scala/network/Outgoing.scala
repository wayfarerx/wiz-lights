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
package network

import protocol.Request

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
  case class Multicast(addresses: Set[InetAddress], request: Request) extends Outgoing

  /**
   * A message that is sent to a single device.
   *
   * @param address The address of the device targeted by this message.
   * @param request The request payload to send.
   */
  case class Unicast(address: InetAddress, request: Request) extends Outgoing
