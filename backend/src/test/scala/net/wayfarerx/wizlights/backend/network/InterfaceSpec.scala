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
import java.nio.ByteBuffer

import zio.Scope
import zio.test.*

/**
 * Specifications for network interfaces.
 */
object InterfaceSpec extends ZIOSpecDefault:

  /**
   * Creates an interface for testing.
   *
   * @param macAddress       The MAC address of the interface.
   * @param ipAddress        The literal IPv4 address of the interface.
   * @param prefixLength     The subnet prefix length of the interface.
   * @param broadcastAddress The optional literal broadcast address of the interface.
   * @return A new interface for testing.
   */
  def interface(
    macAddress: String,
    ipAddress: String,
    prefixLength: Int,
    broadcastAddress: Option[String] = None
  ): Interface =
    val address = InetAddress.ofLiteral(ipAddress)
    Interface(
      macAddress,
      address,
      ByteBuffer.wrap(address.getAddress).getInt,
      prefixLength,
      broadcastAddress.map(InetAddress.ofLiteral)
    )

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("InterfaceSpec")(
    test("key identifies an interface by MAC address, IP address and prefix length") {
      val interface1 = interface("aabbccddeeff", "192.168.1.10", 24, Some("192.168.1.255"))
      val interface2 = interface1.copy(broadcastAddress = None)
      assertTrue(
        interface1.key == ("aabbccddeeff", InetAddress.ofLiteral("192.168.1.10"), 24),
        interface1.key == interface2.key
      )
    },
    test("macAndIpAddresses pairs the MAC address with the textual IP address") {
      assertTrue(interface("aabbccddeeff", "10.0.0.5", 8).macAndIpAddresses == ("aabbccddeeff", "10.0.0.5"))
    }
  )
