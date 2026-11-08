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

import java.net.{Inet4Address, InetAddress, NetworkInterface}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import io.netty.channel.nio.NioIoHandler
import io.netty.channel.{EventLoopGroup, MultiThreadIoEventLoopGroup}
import zio.{Duration, Hub, Queue, RIO, RLayer, Ref, Schedule, Scope, Task, UIO, URIO, ZIO, ZLayer}
import zio.stream.{UStream, ZStream}

/**
 * A service that manages interaction with the network.
 */
trait Network:

  /** Returns the default local MAC address and internet address if available. */
  def localhost: UIO[Option[(String, String)]]

  /**
   * Returns the local MAC address and internet address for communicating with the specified target if available.
   *
   * @param target The destination address of the target device.
   * @return The local MAC address and internet address for the target if available.
   */
  def localhost(target: InetAddress): UIO[Option[(String, String)]]

  /**
   * Publishes a command message on the command port.
   *
   * @param message The outgoing message to publish.
   * @return An effect that completes when the message is published.
   */
  def publish(message: Outgoing): UIO[Unit]

  /**
   * Subscribes to responses received across all response ports.
   *
   * @return A subscription to incoming responses from all response ports.
   */
  def subscribe: URIO[Scope, UStream[Incoming]]

/**
 * The live implementation of the network service.
 *
 * @param interfaces The cached local network interfaces.
 * @param outgoing   The queue of outgoing messages.
 * @param incoming   The hub of incoming messages.
 */
final class NetworkLive private(
  interfaces: Ref[List[LocalInterface]],
  outgoing: Queue[Outgoing],
  incoming: Hub[Incoming]
) extends Network:

  /* Return the default local MAC address and internet address if available. */
  override def localhost: UIO[Option[(String, String)]] =
    interfaces.get.map(_.headOption.map(i => i.macAddress -> i.ipAddress.getHostAddress))

  /* Return the local MAC address and internet address for communicating with the specified target if available. */
  override def localhost(target: InetAddress): UIO[Option[(String, String)]] =
    interfaces.get.map { list =>
      list.find(_.matches(target))
        .orElse(list.headOption)
        .map(i => i.macAddress -> i.ipAddress.getHostAddress)
    }

  /* Publish a command message on the command port. */
  override def publish(message: Outgoing): UIO[Unit] =
    outgoing.offer(message).ignore

  /* Subscribe to responses received across all response ports. */
  override def subscribe: URIO[Scope, UStream[Incoming]] =
    ZStream.fromHubScoped(incoming)

/**
 * Factory for live network services.
 */
object NetworkLive:

  /** The golden ratio for staggering broadcast address refreshes. */
  private val GoldenRatio = (1.0 + Math.sqrt(5.0)) / 2.0

  /** The default broadcast addresses. */
  private val DefaultBroadcastAddresses =
    ZIO.attempt(Set(InetAddress.ofLiteral("255.255.255.255"))).memoize.flatten

  /** The layer that provides the default network service. */
  val layer: RLayer[Configuration, Network] = ZLayer.scoped {
    for
      config <- ZIO.service[Configuration]
      network <- make(
        config.outgoingWindow,
        config.incomingWindow,
        config.commandPort,
        config.listenerPort,
        config.discoveryInterval
      )
    yield network
  }

  /**
   * Creates a live network representation.
   *
   * @param outgoingWindow    The number of outgoing requests to keep in the network queue.
   * @param incomingWindow    The number of incoming responses to keep in the network hub.
   * @param commandPort       The network port to send commands on.
   * @param listenerPort      The network port to also listen on.
   * @param discoveryInterval The interval to wait between discovering devices.
   * @return A live network representation.
   */
  def make(
    outgoingWindow: Int,
    incomingWindow: Int,
    commandPort: Int,
    listenerPort: Int,
    discoveryInterval: FiniteDuration
  ): RIO[Scope, Network] =
    for
      eventLoopGroup <- ZIO.acquireRelease(acquireEventLoopGroup)(releaseEventLoopGroup)
      outgoing <- Queue.sliding[Outgoing](outgoingWindow)
      incoming <- Hub.sliding[Incoming](incomingWindow)
      received = incoming.publish(_: Incoming).ignore
      commandSocket <- Socket.make(commandPort, eventLoopGroup, received)
      listenerSocket <- Socket.make(listenerPort, eventLoopGroup, received)
      refreshNetworkInterval = Duration.fromScala(discoveryInterval * GoldenRatio)
      interfaces <- Ref.make[List[LocalInterface]](Nil)
      broadcastAddresses <- DefaultBroadcastAddresses.flatMap(Ref.make)
      _ <- refreshNetworkInfo(interfaces, broadcastAddresses)
      _ <- refreshNetworkInfo(interfaces, broadcastAddresses)
        .repeat(Schedule.spaced(refreshNetworkInterval)).delay(refreshNetworkInterval).forkScoped
      _ <- ZStream.fromQueue(outgoing).foreach(write(broadcastAddresses, commandSocket, _)).forkScoped
    yield NetworkLive(interfaces, outgoing, incoming)

  /**
   * Acquires an event loop group.
   *
   * @return The acquired event loop group.
   */
  private def acquireEventLoopGroup: Task[EventLoopGroup] =
    ZIO.attemptBlocking(MultiThreadIoEventLoopGroup(NioIoHandler.newFactory))

  /**
   * Releases an event loop group.
   *
   * @param group The event loop group to release.
   */
  private def releaseEventLoopGroup(group: EventLoopGroup): UIO[Unit] =
    ZIO.attemptBlocking(group.close()).ignore

  /**
   * Refreshes the cached network information.
   *
   * @param interfacesRef         The reference to the current local interfaces.
   * @param broadcastAddressesRef The reference to the current set of broadcast addresses.
   */
  private def refreshNetworkInfo(
    interfacesRef: Ref[List[LocalInterface]],
    broadcastAddressesRef: Ref[Set[InetAddress]]
  ): UIO[Unit] = for
    interfaces <- ZIO.attempt(Option(NetworkInterface.getNetworkInterfaces).fold(Nil)(_.asScala.toSeq))
      .catchAllCause(ZIO.logInfoCause("Cannot list the network interfaces.", _).map(_ => Seq.empty[NetworkInterface]))
    availableInterfaces <- ZIO.filter(interfaces) { i =>
      ZIO.attempt(!i.isLoopback && i.isUp).catchAllCause {
        ZIO.logInfoCause(s"Unable to query the network interface: ${i.getName}.", _).map(_ => false)
      }
    }
    fallbackMac = availableInterfaces.iterator.flatMap { i =>
      Option(i.getHardwareAddress).filter(_.length == 6).map(java.util.HexFormat.of().formatHex(_))
    }.nextOption()
    localInterfaces = availableInterfaces.flatMap { i =>
      val macOpt = Option(i.getHardwareAddress).filter(_.length == 6).map(java.util.HexFormat.of().formatHex(_)).orElse(fallbackMac)
      macOpt.toList.flatMap { mac =>
        i.getInterfaceAddresses.asScala.flatMap { ifAddr =>
          ifAddr.getAddress match
            case ip4: Inet4Address =>
              val hasBroadcast = ifAddr.getBroadcast != null
              Some((hasBroadcast, LocalInterface(mac, ip4, ifAddr.getNetworkPrefixLength)))
            case _ => None
        }
      }
    }.sortBy(!_._1).map(_._2).toList
    _ <- interfacesRef.set(localInterfaces)
    interfaceAddresses = availableInterfaces.flatMap(_.getInterfaceAddresses.asScala)
    broadcastAddresses <- interfaceAddresses.flatMap(Option apply _.getBroadcast) match
      case Nil => DefaultBroadcastAddresses.catchAllCause {
        ZIO.logWarningCause("Cannot find any broadcast addresses.", _) map (_ => Set.empty)
      }
      case some => ZIO.succeed(some.toSet)
    _ <- broadcastAddressesRef.set(broadcastAddresses)
  yield ()

  /**
   * Writes a message to the specified socket.
   *
   * @param broadcastAddresses The live collection of available broadcast addresses.
   * @param socket             The socket to write to.
   * @param message            The message to write to the socket.
   */
  private def write(broadcastAddresses: Ref[Set[InetAddress]], socket: Socket, message: Outgoing): UIO[Unit] = for
    addresses <- message match
      case Outgoing.Broadcast(_) => broadcastAddresses.get
      case Outgoing.Send(addresses, _) => ZIO.succeed(addresses)
    _ <- socket(addresses, message.request)
  yield ()

