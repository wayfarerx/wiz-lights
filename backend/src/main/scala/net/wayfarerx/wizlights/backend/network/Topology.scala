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
 * @param interfaces       The active local network interfaces.
 * @param defaultBroadcast The default broadcast addresses to use when none are discovered.
 */
case class Topology(
  interfaces: List[Interface] = Nil,
  defaultBroadcast: Set[InetAddress] = Topology.DefaultBroadcastAddresses
):

  /** The available broadcast addresses. */
  def broadcastAddresses: Set[InetAddress] =
    val discovered = interfaces.flatMap(_.broadcastAddress).toSet
    if discovered.nonEmpty then discovered else defaultBroadcast

  /** Returns the default local network interface if available. */
  def localInterface: Option[Interface] =
    interfaces.headOption

  /**
   * Returns the local network interface for communicating with the specified target if available.
   *
   * @param target The destination address of the target device.
   * @return The local network interface for communicating with the target if available.
   */
  def localInterface(target: InetAddress): Option[Interface] =
    interfaces.find(_.targets(target)).orElse(localInterface)

  /** Returns the default local MAC address and internet address if available. */
  def localhost: Option[(String, String)] =
    localInterface.map(i => i.macAddress -> i.ipAddress.getHostAddress)

  /**
   * Returns the local MAC address and internet address for communicating with the specified target if available.
   *
   * @param target The destination address of the target device.
   * @return The local MAC address and internet address for the target if available.
   */
  def localhost(target: InetAddress): Option[(String, String)] =
    localInterface(target).map(i => i.macAddress -> i.ipAddress.getHostAddress)

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
 * Definitions associated with network topologies.
 */
object Topology:

  /** The default broadcast addresses. */
  val DefaultBroadcastAddresses: Set[InetAddress] =
    Set(InetAddress.ofLiteral("255.255.255.255"))

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
        for mac <- getMacIfUpAndNotLoopback(interface) yield
          mac.fold(interfaces) { mac =>
            interfaces ++ interface.getInterfaceAddresses.asScala.toVector.flatMap(Interface.from(mac, _))
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
