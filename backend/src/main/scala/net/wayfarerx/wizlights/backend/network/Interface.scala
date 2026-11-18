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
case class Interface private(
  macAddress: String,
  ipAddress: InetAddress,
  prefixLength: Int,
  broadcastAddress: Option[InetAddress] = None
):

  /** The MAC address and IP address of this interface. */
  lazy val macAndIpAddresses: (String, String) = macAddress -> ipAddress.getHostAddress

/**
 * Definitions associated with local network interfaces.
 */
object Interface:

  /**
   * Attempts to create a local network interface from a MAC address and interface address.
   *
   * @param macAddress       The MAC address of the interface.
   * @param interfaceAddress The interface address to inspect.
   * @return The local network interface if the address is a valid IPv4 address.
   */
  def from(macAddress: String, interfaceAddress: InterfaceAddress): Option[Interface] =
    Option(interfaceAddress.getAddress).filter(_.getAddress.length == 4).map { inetAddress =>
      Interface(macAddress, inetAddress, interfaceAddress.getNetworkPrefixLength, Option(interfaceAddress.getBroadcast))
    }
