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

import java.net.InetAddress

/**
 * Base type for changes to the local network interfaces between versions of the network topology.
 *
 * A local network interface is identified by its MAC address and IP address.
 */
sealed trait Event:

  /** The MAC address of the affected interface in hexadecimal. */
  def macAddress: String

  /** The IP address of the affected interface in the topology where it is present, preferring the new topology. */
  def ipAddress: InetAddress

/**
 * Definitions of the supported network topology events.
 */
object Event:

  /**
   * An interface that is present in the new topology but not in the old one.
   *
   * @param macAddress   The MAC address of the added interface in hexadecimal.
   * @param ipAddress    The IP address of the added interface.
   * @param prefixLength The subnet prefix length of the added interface.
   */
  case class Added(macAddress: String, ipAddress: InetAddress, prefixLength: Int) extends Event

  /**
   * An interface that is present in the old topology but not in the new one.
   *
   * @param macAddress The MAC address of the removed interface in hexadecimal.
   * @param ipAddress  The IP address of the removed interface.
   */
  case class Removed(macAddress: String, ipAddress: InetAddress) extends Event

  /**
   * An interface that is present in both topologies but whose subnet prefix length has changed.
   *
   * @param macAddress   The MAC address of the resized interface in hexadecimal.
   * @param ipAddress    The IP address of the resized interface.
   * @param prefixLength The new subnet prefix length of the resized interface.
   */
  case class Resized(macAddress: String, ipAddress: InetAddress, prefixLength: Int) extends Event

  /**
   * An interface whose IP address in the old topology has been replaced by a new IP address in the new one.
   *
   * @param macAddress        The MAC address of the readdressed interface in hexadecimal.
   * @param previousIpAddress The IP address of the readdressed interface in the old topology.
   * @param ipAddress         The IP address of the readdressed interface in the new topology.
   * @param prefixLength      The subnet prefix length of the readdressed interface in the new topology.
   */
  case class Readdressed(
    macAddress: String,
    previousIpAddress: InetAddress,
    ipAddress: InetAddress,
    prefixLength: Int
  ) extends Event
