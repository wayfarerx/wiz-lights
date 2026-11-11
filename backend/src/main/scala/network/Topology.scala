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

import scala.jdk.CollectionConverters.*

import zio.{IO, ZIO}

/**
 * The topology of the local network.
 *
 * @param interfaces       The active local network interfaces.
 * @param defaultBroadcast The default broadcast addresses to use when none are discovered.
 */
case class Topology(
  interfaces: List[Local] = Nil,
  defaultBroadcast: Set[InetAddress] = Topology.DefaultBroadcastAddresses
):

  /** The available broadcast addresses. */
  def broadcastAddresses: Set[InetAddress] =
    val discovered = interfaces.flatMap(_.broadcastAddress).toSet
    if discovered.nonEmpty then discovered else defaultBroadcast

  /** Returns the default local network interface if available. */
  def localInterface: Option[Local] =
    interfaces.headOption

  /**
   * Returns the local network interface for communicating with the specified target if available.
   *
   * @param target The destination address of the target device.
   * @return The local network interface for communicating with the target if available.
   */
  def localInterface(target: InetAddress): Option[Local] =
    interfaces.find(_.matches(target)).orElse(localInterface)

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

  /** Discovers the local network topology. */
  val discover: IO[SocketException, Topology] =
    ZIO.attemptBlocking(Option(NetworkInterface.getNetworkInterfaces).fold(Nil)(_.asScala.toList))
      .refineToOrDie[SocketException]
      .flatMap(make(_))

  /**
   * Creates a network topology from the specified network interfaces.
   *
   * @param interfaces       The network interfaces to inspect.
   * @param defaultBroadcast The default broadcast addresses to use if none are found.
   * @return The constructed network topology or a socket exception if an error occurs.
   */
  def from(
    interfaces: Iterable[NetworkInterface],
    defaultBroadcast: Set[InetAddress] = DefaultBroadcastAddresses
  ): Either[SocketException, Topology] =
    try
      val available = interfaces.filter(i => !i.isLoopback && i.isUp).toList
      val fallbackMac = available.iterator.flatMap(macOf).nextOption()
      val locals = available.flatMap { iface =>
        val mac = macOf(iface).orElse(fallbackMac)
        mac.toList.flatMap { m =>
          iface.getInterfaceAddresses.asScala.flatMap(Local.from(m, _))
        }
      }.sortBy(!_.canBroadcast)
      Right(Topology(locals, defaultBroadcast))
    catch
      case e: SocketException => Left(e)

  /**
   * Creates a network topology from the specified network interfaces.
   *
   * @param interfaces       The network interfaces to inspect.
   * @param defaultBroadcast The default broadcast addresses to use if none are found.
   * @return The constructed network topology.
   */
  def make(
    interfaces: Iterable[NetworkInterface],
    defaultBroadcast: Set[InetAddress] = DefaultBroadcastAddresses
  ): IO[SocketException, Topology] =
    ZIO.fromEither(from(interfaces, defaultBroadcast))

  /**
   * Returns the hexadecimal MAC address for an interface if valid.
   *
   * @param i The network interface to inspect.
   * @return The MAC address in hexadecimal if available.
   */
  private def macOf(i: NetworkInterface): Option[String] =
    Option(i.getHardwareAddress).filter(_.length == 6).map(java.util.HexFormat.of().formatHex(_))
