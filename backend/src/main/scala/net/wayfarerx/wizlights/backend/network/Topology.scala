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

import java.net.{InetAddress, NetworkInterface, SocketException}
import java.util.HexFormat

import scala.jdk.CollectionConverters.*

import zio.{IO, UIO, ZIO}

/**
 * The topology of the local network.
 *
 * @param interfaces                The active network interfaces.
 * @param defaultBroadcastAddresses The default broadcast addresses to use when none are discovered.
 */
private[network] case class Topology private(
  interfaces: List[Interface] = Nil,
  defaultBroadcastAddresses: Set[InetAddress] = Topology.DefaultBroadcastAddresses
):

  /** The available broadcast addresses. */
  private lazy val broadcastAddresses: Set[InetAddress] =
    interfaces.flatMap(_.broadcastAddress) match
      case Nil => defaultBroadcastAddresses
      case all if all.size == interfaces.size => all.toSet
      case some => some.toSet ++ defaultBroadcastAddresses

  /**
   * The IPv4 interfaces indexed by subnet, as pairs of subnet mask and network address to interface, ordered from the
   * longest prefix to the shortest. Within a subnet the first interface in list order is retained.
   */
  private lazy val subnets: Vector[(Int, Map[Int, Interface])] =
    interfaces.foldRight(Map.empty[Int, Map[Int, Interface]]) { (interface, result) =>
      ipv4Address(interface.ipAddress).fold(result) { address =>
        val prefixLength = Math.max(0, Math.min(32, interface.prefixLength))
        val networks = result.getOrElse(prefixLength, Map.empty)
        result.updated(prefixLength, networks.updated(address & subnetMaskFor(prefixLength), interface))
      }
    }.toVector.sortBy(-_._1).map((prefixLength, networks) => subnetMaskFor(prefixLength) -> networks)

  /**
   * Returns the local network interface for communicating with the specified target if available.
   *
   * @param target The destination address of the target device.
   * @return The local network interface for communicating with the target if available.
   */
  def interfaceFor(target: InetAddress): Option[Interface] =
    ipv4Address(target).flatMap { ipv4Address =>
      subnets.iterator.flatMap((mask, networks) => networks.get(ipv4Address & mask)).nextOption()
    }

  /**
   * Resolves the destination addresses for an outgoing message.
   *
   * @param message The outgoing message to resolve destinations for.
   * @return The destination addresses for the message.
   */
  def destinations(message: Outgoing): Set[InetAddress] = message match
    case Outgoing.Broadcast(_) => broadcastAddresses
    case Outgoing.Send(addresses, _) => addresses

  /**
   * Returns the events that describe the changes to the interfaces between this network topology and a newer one,
   * with all deactivations preceding all activations. Changes to broadcast addresses are ignored.
   *
   * @param newTopology The newer network topology.
   * @return The events that describe the changes to the interfaces between this and the newer network topology.
   */
  def diff(newTopology: Topology): List[Event] =
    val oldInterfaces = interfaces.map(i => (i.macAddress, i.ipAddress, i.prefixLength)).distinct
    val newInterfaces = newTopology.interfaces.map(i => (i.macAddress, i.ipAddress, i.prefixLength)).distinct
    val deactivated = oldInterfaces.filterNot(newInterfaces.toSet).map(Event.Deactivated(_, _, _))
    val activated = newInterfaces.filterNot(oldInterfaces.toSet).map(Event.Activated(_, _, _))
    deactivated ++ activated

/**
 * Definitions associated with network topologies.
 */
private[network] object Topology:

  /** The default broadcast addresses. */
  val DefaultBroadcastAddresses: Set[InetAddress] = Set(InetAddress.ofLiteral("255.255.255.255"))

  /**
   * Discovers the local network topology.
   *
   * @param defaultBroadcastAddresses The default broadcast addresses to use if none are found.
   */
  def discover(defaultBroadcastAddresses: Set[InetAddress] = DefaultBroadcastAddresses): IO[SocketException, Topology] =
    for
      interfaces <- ZIO.attemptBlocking[Seq[NetworkInterface]](
        Option(NetworkInterface.getNetworkInterfaces).fold(Seq.empty)(_.asScala.toSeq)
      ).refineToOrDie[SocketException]
      result <- make(interfaces, defaultBroadcastAddresses)
    yield result

  /**
   * Creates a network topology from the specified network interfaces.
   *
   * @param networkInterfaces          The network interfaces to inspect.
   * @param defaultBroadcastAddresses  The default broadcast addresses to use if none are found.
   * @return The constructed network topology.
   */
  def make(
    networkInterfaces: Iterable[NetworkInterface],
    defaultBroadcastAddresses: Set[InetAddress] = DefaultBroadcastAddresses
  ): UIO[Topology] =
    for
      interfaces <- ZIO.foldLeft(networkInterfaces)(Vector.empty[Interface]) { (interfaces, interface) =>
        for macAddress <- getMacIfUpAndNotLoopback(interface) yield
          macAddress.fold(interfaces) { macAddress =>
            interfaces ++ interface.getInterfaceAddresses.asScala.toVector.flatMap(Interface.from(macAddress, _))
          }
      }
    yield Topology(interfaces.toList, defaultBroadcastAddresses)

  /**
   * Inspects a network address and returns its MAC address if it is up and not the loopback address.
   *
   * @param interface The network interface to inspect.
   * @return The MAC address of the network interface if it is up and not the loopback address.
   */
  private def getMacIfUpAndNotLoopback(interface: NetworkInterface): UIO[Option[String]] =
    for
      hardwareAddress <- ZIO.attemptBlocking {
        if interface.isUp && !interface.isLoopback then Option(interface.getHardwareAddress) else None
      }.catchAllCause(
        ZIO.logWarningCause(s"Failed to inspect network interface: ${interface.getDisplayName}.", _).map(_ => None)
      )
    yield hardwareAddress.filter(_.length == 6).map(HexFormat.of.formatHex(_).toLowerCase)
