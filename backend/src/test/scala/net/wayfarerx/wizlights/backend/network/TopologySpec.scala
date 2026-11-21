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

import java.net.{InetAddress, InterfaceAddress, NetworkInterface, SocketException}

import org.mockito.Mockito.{mock, when}

import zio.{LogLevel, Scope, ZIO}
import zio.test.*

import protocol.GetPilotRequest

import InterfaceSpec.interface

/**
 * Specifications for network topologies.
 */
object TopologySpec extends ZIOSpecDefault:

  /**
   * Creates a mock network interface.
   *
   * @param up              True if the interface is up.
   * @param loopback        True if the interface is the loopback interface.
   * @param hardwareAddress The hardware address of the interface.
   * @param addresses       The addresses bound to the interface.
   * @return A mock network interface.
   */
  def networkInterface(
    up: Boolean = true,
    loopback: Boolean = false,
    hardwareAddress: Array[Byte] | Null = Array(0x00, 0xaa, 0xbb, 0xcc, 0xdd, 0xee).map(_.toByte),
    addresses: List[InterfaceAddress] = Nil
  ): NetworkInterface =
    val result = mock(classOf[NetworkInterface])
    when(result.isUp).thenReturn(up)
    when(result.isLoopback).thenReturn(loopback)
    when(result.getHardwareAddress).thenReturn(hardwareAddress)
    when(result.getInterfaceAddresses).thenReturn(java.util.List.of(addresses*))
    result

  /**
   * Creates a mock interface address.
   *
   * @param address          The literal address, if any.
   * @param prefixLength     The network prefix length.
   * @param broadcastAddress The literal broadcast address, if any.
   * @return A mock interface address.
   */
  def interfaceAddress(
    address: String | Null,
    prefixLength: Int = 24,
    broadcastAddress: String | Null = null
  ): InterfaceAddress =
    val result = mock(classOf[InterfaceAddress])
    when(result.getAddress).thenReturn(if address == null then null else InetAddress.ofLiteral(address))
    when(result.getNetworkPrefixLength).thenReturn(prefixLength.toShort)
    when(result.getBroadcast)
      .thenReturn(if broadcastAddress == null then null else InetAddress.ofLiteral(broadcastAddress))
    result

  /** A wide interface on the 192.168.0.0/16 subnet. */
  val wide: Interface = interface("aabbccddeeff", "192.168.1.10", 16, Some("192.168.255.255"))

  /** A narrow interface on the 192.168.1.0/24 subnet. */
  val narrow: Interface = interface("aabbccddeeff", "192.168.1.20", 24, Some("192.168.1.255"))

  /** Another narrow interface on the 192.168.1.0/24 subnet. */
  val narrowToo: Interface = interface("112233445566", "192.168.1.30", 24)

  /** An interface on the 10.0.0.0/8 subnet without a broadcast address. */
  val other: Interface = interface("112233445566", "10.0.0.5", 8)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("TopologySpec")(
    suite("interfaceFor")(
      test("prefer the interface with the longest matching prefix") {
        val topology = Topology(List(wide, narrow, other))
        assertTrue(
          topology.interfaceFor(InetAddress.ofLiteral("192.168.1.50")).contains(narrow),
          topology.interfaceFor(InetAddress.ofLiteral("192.168.7.50")).contains(wide),
          topology.interfaceFor(InetAddress.ofLiteral("10.20.30.40")).contains(other)
        )
      },
      test("prefer the first interface in list order when prefixes are equal") {
        assertTrue(
          Topology(List(narrow, narrowToo)).interfaceFor(InetAddress.ofLiteral("192.168.1.50")).contains(narrow),
          Topology(List(narrowToo, narrow)).interfaceFor(InetAddress.ofLiteral("192.168.1.50")).contains(narrowToo)
        )
      },
      test("match any IPv4 target with a zero length prefix") {
        val everything = interface("aabbccddeeff", "172.16.0.1", 0)
        assertTrue(Topology(List(everything)).interfaceFor(InetAddress.ofLiteral("8.8.8.8")).contains(everything))
      },
      test("return nothing for unmatched or IPv6 targets") {
        val topology = Topology(List(wide, narrow, other))
        assertTrue(
          topology.interfaceFor(InetAddress.ofLiteral("172.16.0.1")).isEmpty,
          topology.interfaceFor(InetAddress.ofLiteral("fe80::1")).isEmpty,
          Topology.Empty.interfaceFor(InetAddress.ofLiteral("192.168.1.50")).isEmpty
        )
      }
    ),
    suite("destinations")(
      test("broadcast to the interface broadcast addresses") {
        assertTrue(
          Topology(List(wide, narrow, other)).destinations(Outgoing.Broadcast(GetPilotRequest())) ==
            Set(InetAddress.ofLiteral("192.168.255.255"), InetAddress.ofLiteral("192.168.1.255"))
        )
      },
      test("broadcast to the default broadcast addresses when no interface has a broadcast address") {
        assertTrue(
          Topology(List(other)).destinations(Outgoing.Broadcast(GetPilotRequest())) ==
            Topology.DefaultBroadcastAddresses,
          Topology.Empty.destinations(Outgoing.Broadcast(GetPilotRequest())) ==
            Set(InetAddress.ofLiteral("255.255.255.255"))
        )
      },
      test("send to the explicit addresses") {
        val addresses = Set(InetAddress.ofLiteral("192.168.1.50"), InetAddress.ofLiteral("10.0.0.9"))
        assertTrue(Topology(List(wide)).destinations(Outgoing.Send(addresses, GetPilotRequest())) == addresses)
      }
    ),
    suite("diff")(
      test("emit deactivations before activations") {
        assertTrue(
          Topology(List(wide, narrow)).diff(Topology(List(narrow, other))) == List(
            Event.Deactivated(wide.macAddress, wide.ipAddress, wide.prefixLength),
            Event.Activated(other.macAddress, other.ipAddress, other.prefixLength)
          )
        )
      },
      test("emit nothing for identical topologies or broadcast address changes") {
        assertTrue(
          Topology(List(wide, narrow)).diff(Topology(List(narrow, wide))).isEmpty,
          Topology(List(narrow)).diff(Topology(List(narrow.copy(broadcastAddress = None)))).isEmpty,
          Topology.Empty.diff(Topology.Empty).isEmpty
        )
      },
      test("emit one event per distinct interface") {
        assertTrue(
          Topology.Empty.diff(Topology(List(narrow, narrow.copy(broadcastAddress = None)))) ==
            List(Event.Activated(narrow.macAddress, narrow.ipAddress, narrow.prefixLength)),
          Topology(List(narrow, narrow)).diff(Topology.Empty) ==
            List(Event.Deactivated(narrow.macAddress, narrow.ipAddress, narrow.prefixLength))
        )
      }
    ),
    suite("make")(
      test("collect the IPv4 addresses of interfaces that are up and not loopback") {
        val networkInterfaces = List(
          networkInterface(addresses = List(
            interfaceAddress("192.168.1.10", 24, "192.168.1.255"),
            interfaceAddress("fe80::1", 64),
            interfaceAddress(null),
            interfaceAddress("10.0.0.5", -1),
            interfaceAddress("172.16.0.1", 40)
          )),
          networkInterface(loopback = true, addresses = List(interfaceAddress("127.0.0.1", 8))),
          networkInterface(up = false, addresses = List(interfaceAddress("192.168.2.10"))),
          networkInterface(hardwareAddress = null, addresses = List(interfaceAddress("192.168.3.10"))),
          networkInterface(hardwareAddress = Array.fill[Byte](8)(1), addresses = List(interfaceAddress("192.168.4.10")))
        )
        for
          topology <- Topology.make(networkInterfaces)
        yield assertTrue(
          topology == Topology(List(
            interface("00aabbccddee", "192.168.1.10", 24, Some("192.168.1.255")),
            interface("00aabbccddee", "10.0.0.5", 0),
            interface("00aabbccddee", "172.16.0.1", 32)
          ))
        )
      },
      test("skip and log interfaces that cannot be inspected") {
        val broken = networkInterface(addresses = List(interfaceAddress("192.168.1.10")))
        when(broken.isUp).thenThrow(SocketException("boom"))
        when(broken.getDisplayName).thenReturn("broken0")
        for
          topology <- Topology.make(List(broken))
          logs <- ZTestLogger.logOutput
        yield assertTrue(
          topology == Topology.Empty,
          logs.exists { entry =>
            entry.logLevel == LogLevel.Warning && entry.message() == "Failed to inspect network interface: broken0."
          }
        )
      }
    ),
    test("discover the local network topology") {
      for
        topology <- Topology.discover
      yield assertTrue(topology.interfaces.forall(i => i.prefixLength >= 0 && i.prefixLength <= 32))
    }
  )
