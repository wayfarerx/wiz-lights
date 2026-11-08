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

import java.net.{Inet4Address, InetAddress}
import java.nio.ByteBuffer

/**
 * Information about a local network interface.
 *
 * @param macAddress   The MAC address of the interface in hexadecimal.
 * @param ipAddress    The IPv4 address of the interface.
 * @param prefixLength The subnet prefix length (e.g., 24 for a /24 subnet).
 */
case class LocalInterface(
  macAddress: String,
  ipAddress: Inet4Address,
  prefixLength: Short
):

  /**
   * Returns true if the specified target internet address is on the same subnet as this interface.
   *
   * @param target The target internet address to test.
   * @return True if the target internet address is on the same subnet as this interface.
   */
  def matches(target: InetAddress): Boolean = target match
    case target4: Inet4Address =>
      val targetInt = ByteBuffer.wrap(target4.getAddress).getInt
      val localInt = ByteBuffer.wrap(ipAddress.getAddress).getInt
      val mask = if prefixLength <= 0 then 0 else if prefixLength >= 32 then -1 else -1 << (32 - prefixLength)
      (targetInt & mask) == (localInt & mask)
    case _ => false
