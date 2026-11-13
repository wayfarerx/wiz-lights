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

/**
 * Information about a local network interface.
 *
 * @param macAddress       The MAC address of the interface in hexadecimal.
 * @param ipAddress        The IP address of the interface.
 * @param prefixLength     The subnet prefix length (e.g., 24 for a /24 subnet).
 * @param broadcastAddress The optional broadcast address of the interface subnet.
 */
case class Interface(
  macAddress: String,
  ipAddress: InetAddress,
  prefixLength: Int,
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
  def targets(target: InetAddress): Boolean =
    val targetBytes = target.getAddress
    val localBytes = ipAddress.getAddress
    if targetBytes.length == 4 && localBytes.length == 4 then
      val mask = prefixLength match
        case pl if pl <= 0  => 0
        case pl if pl >= 32 => -1
        case pl             => -1 << (32 - pl)
      (bytesToInt(targetBytes) & mask) == (bytesToInt(localBytes) & mask)
    else false

  /**
   * Converts a 4-byte array to an Int without allocation.
   *
   * @param bytes The 4-byte array to convert.
   * @return The integer representation of the bytes.
   */
  private def bytesToInt(bytes: Array[Byte]): Int =
    ((bytes(0) & 0xff) << 24) | ((bytes(1) & 0xff) << 16) | ((bytes(2) & 0xff) << 8) | (bytes(3) & 0xff)

/**
 * Definitions associated with local network interfaces.
 */
object Interface:

  /**
   * Attempts to create a local network interface from a MAC address and interface address.
   *
   * @param mac    The MAC address of the interface.
   * @param ifAddr The interface address to inspect.
   * @return The local network interface if the address is a valid IPv4 address.
   */
  def from(mac: String, ifAddr: InterfaceAddress): Option[Interface] =
    Option(ifAddr.getAddress).filter(_.getAddress.length == 4).map { addr =>
      Interface(mac, addr, ifAddr.getNetworkPrefixLength, Option(ifAddr.getBroadcast))
    }
