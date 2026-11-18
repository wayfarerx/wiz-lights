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
 * Returns the subnet mask for the specified prefix length.
 *
 * @param prefixLength The subnet prefix length in the range 0 - 32.
 * @return The subnet mask for the prefix length.
 */
private[network] def subnetMaskFor(prefixLength: Int): Int =
  if prefixLength == 0 then 0 else -1 << (32 - prefixLength)

/**
 * Converts an internet address to an integer if it is an IPv4 address.
 *
 * @param address The internet address to convert.
 * @return The integer representation of the address if it is an IPv4 address.
 */
private[network] def ipv4Address(address: InetAddress): Option[Int] =
  val bytes = address.getAddress
  if bytes.length == 4 then
    Some(((bytes(0) & 0xff) << 24) | ((bytes(1) & 0xff) << 16) | ((bytes(2) & 0xff) << 8) | (bytes(3) & 0xff))
  else None
