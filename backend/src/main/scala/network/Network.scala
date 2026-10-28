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

  /** Returns the local MAC address and internet address if available. */
  def localhost: UIO[Option[(String, String)]]

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
 * @param local The cached local host MAC address and internet address.
 * @param outgoing The queue of outgoing messages.
 * @param incoming The hub of incoming messages.
 */
final class NetworkLive private(
  local: Ref[Option[(String, String)]],
  outgoing: Queue[Outgoing],
  incoming: Hub[Incoming]
) extends Network:

  /* Return the local MAC address and internet address if available. */
  override def localhost: UIO[Option[(String, String)]] =
    local.get

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
      localhost <- Ref.make[Option[(String, String)]](None)
      broadcastAddresses <- DefaultBroadcastAddresses.flatMap(Ref.make)
      _ <- refreshNetworkInfo(localhost, broadcastAddresses)
      _ <- refreshNetworkInfo(localhost, broadcastAddresses)
        .repeat(Schedule.spaced(refreshNetworkInterval)).delay(refreshNetworkInterval).forkScoped
      _ <- ZStream.fromQueue(outgoing).foreach(write(broadcastAddresses, commandSocket, _)).forkScoped
    yield NetworkLive(localhost, outgoing, incoming)

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
   * @param localhostRef The reference to the current local host information.
   * @param broadcastAddressesRef The reference to the current set of broadcast addresses.
   */
  private def refreshNetworkInfo(
    localhostRef: Ref[Option[(String, String)]],
    broadcastAddressesRef: Ref[Set[InetAddress]]
  ): UIO[Unit] = for
    interfaces <- ZIO.attempt(Option(NetworkInterface.getNetworkInterfaces).fold(Nil)(_.asScala.toSeq))
      .catchAllCause(ZIO.logInfoCause("Cannot list the network interfaces.", _).map(_ => Seq.empty[NetworkInterface]))
    availableInterfaces <- ZIO.filter(interfaces) { i =>
      ZIO.attempt(!i.isLoopback && i.isUp).catchAllCause {
        ZIO.logInfoCause(s"Unable to query the network interface: ${i.getName}.", _).map(_ => false)
      }
    }
    localhost <- ZIO.collectFirst(availableInterfaces) { i =>
      for
        mac <- ZIO.attempt(i.getHardwareAddress).map {
          Option(_).filter(_.length == 6).map(java.util.HexFormat.of().formatHex(_))
        }.catchAllCause(ZIO.logInfoCause(s"Failed to find the hardware address of: ${i.getName}.", _).map (_ => None))
      yield mac.flatMap { m =>
        i.getInetAddresses.asScala.find {
          case _: Inet4Address => true
          case _ => false
        }.map(m -> _.getHostAddress)
      }
    }
    _ <- localhostRef.set(localhost)
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

