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
 * Information about a local network interface.
 *
 * @param macAddress       The MAC address of the interface in hexadecimal.
 * @param ipAddress        The IPv4 address of the interface.
 * @param ipv4Address      The integer representation of the IPv4 address of the interface.
 * @param prefixLength     The subnet prefix length in the range 0 - 32 (e.g., 24 for a /24 subnet).
 * @param broadcastAddress The optional broadcast address of this interface.
 */
private[network] case class Interface(
  macAddress: String,
  ipAddress: InetAddress,
  ipv4Address: Int,
  prefixLength: Int,
  broadcastAddress: Option[InetAddress]
):

  /** The unique key of this interface. */
  lazy val key: (String, InetAddress, Int) = (macAddress, ipAddress, prefixLength)

  /** The MAC address and IP address of this interface. */
  lazy val macAndIpAddresses: (String, String) = macAddress -> ipAddress.getHostAddress
