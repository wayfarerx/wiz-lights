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

import java.net.{InetAddress, InterfaceAddress}
import java.nio.ByteBuffer

/**
 * Information about a local network interface.
 *
 * @param macAddress       The MAC address of the interface in hexadecimal.
 * @param ipAddress        The IP address of the interface.
 * @param prefixLength     The subnet prefix length (e.g., 24 for a /24 subnet).
 * @param broadcastAddress The optional broadcast address of the interface subnet.
 */
case class Local(
  macAddress: String,
  ipAddress: InetAddress,
  prefixLength: Short,
  broadcastAddress: Option[InetAddress] = None
):

  /** Returns true if this interface has an associated broadcast address. */
  def canBroadcast: Boolean = broadcastAddress.isDefined

  /**
   * Returns true if the specified target internet address is on the same subnet as this interface.
   *
   * @param target The target internet address to test.
   * @return True if the target internet address is on the same subnet as this interface.
   */
  def matches(target: InetAddress): Boolean =
    val targetBytes = target.getAddress
    val localBytes = ipAddress.getAddress
    if targetBytes.length == 4 && localBytes.length == 4 then
      val targetInt = ByteBuffer.wrap(targetBytes).getInt
      val localInt = ByteBuffer.wrap(localBytes).getInt
      val mask = if prefixLength <= 0 then 0 else if prefixLength >= 32 then -1 else -1 << (32 - prefixLength)
      (targetInt & mask) == (localInt & mask)
    else false

/**
 * Definitions associated with local network interfaces.
 */
object Local:

  /**
   * Attempts to create a local network interface from a MAC address and interface address.
   *
   * @param mac    The MAC address of the interface.
   * @param ifAddr The interface address to inspect.
   * @return The local network interface if the address is a valid IPv4 address.
   */
  def from(mac: String, ifAddr: InterfaceAddress): Option[Local] =
    val addr = ifAddr.getAddress
    if addr != null && addr.getAddress.length == 4 then
      Some(Local(mac, addr, ifAddr.getNetworkPrefixLength, Option(ifAddr.getBroadcast)))
    else None
