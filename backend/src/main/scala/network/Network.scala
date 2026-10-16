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

import java.net.{InetAddress, NetworkInterface}

import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import io.netty.channel.nio.NioIoHandler
import io.netty.channel.{EventLoopGroup, MultiThreadIoEventLoopGroup}

import zio.{Duration, Hub, Queue, RIO, RLayer, Ref, Schedule, Scope, Task, UIO, URIO, ZIO, ZLayer}
import zio.stream.{UStream, ZStream}

/**
 * A service that manages interaction with the network.
 *
 * @param outgoing The queue of outgoing messages.
 * @param incoming The hub of incoming messages.
 */
final class Network private(outgoing: Queue[Outgoing], incoming: Hub[Incoming]):

  /**
   * Publishes a command message on the command port.
   *
   * @param message The outgoing message to publish.
   * @return An effect that completes when the message is published.
   */
  def publish(message: Outgoing): UIO[Unit] =
    outgoing.offer(message).ignore

  /**
   * Subscribes to responses received across all response ports.
   *
   * @return A subscription to incoming responses from all response ports.
   */
  def subscribe: URIO[Scope, UStream[Incoming]] =
    ZStream.fromHubScoped(incoming)

/**
 * Factory for network services.
 */
object Network:

  /** The default broadcast addresses. */
  private val DefaultBroadcastAddresses =
    ZIO.attempt(Set(InetAddress.ofLiteral("255.255.255.255"))).orDie.memoize.flatten

  /** The layer that provides the default network service. */
  val layer: RLayer[Configuration, Network] = ZLayer.scoped {
    for
      config <- ZIO.service[Configuration]
      network <- make(config.commandPort, config.listenerPort, config.discoveryInterval)
    yield network
  }

  /**
   * Creates a live network representation.
   *
   * @param commandPort       The network port to send commands on.
   * @param listenerPort      The network port to also listen on.
   * @param discoveryInterval The interval to wait between discovering devices.
   * @return A live network representation.
   */
  def make(commandPort: Int, listenerPort: Int, discoveryInterval: FiniteDuration): RIO[Scope, Network] =
    for
      eventLoopGroup <- ZIO.acquireRelease(acquireEventLoopGroup)(releaseEventLoopGroup)
      outgoing <- Queue.unbounded[Outgoing]
      incoming <- Hub.unbounded[Incoming]
      received = incoming.publish(_: Incoming).ignore
      commandSocket <- Socket.make(commandPort, eventLoopGroup, received)
      listenerSocket <- Socket.make(listenerPort, eventLoopGroup, received)
      broadcastAddresses <- DefaultBroadcastAddresses.flatMap(Ref.make)
      _ <- refreshBroadcastAddresses(broadcastAddresses)
        .repeat(Schedule.fixed(Duration.fromScala(discoveryInterval * Math.E))).forkScoped
      _ <- ZStream.fromQueue(outgoing).foreach(write(broadcastAddresses, commandSocket, _)).forkScoped
    yield Network(outgoing, incoming)

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
   * Refreshes the specified set of broadcast addresses.
   *
   * @param broadcastAddressesRef The reference to the current set of broadcast addresses.
   */
  private def refreshBroadcastAddresses(broadcastAddressesRef: Ref[Set[InetAddress]]): UIO[Unit] = for
    interfaces <- ZIO.attempt(Option(NetworkInterface.getNetworkInterfaces).fold(Nil)(_.asScala.toSeq))
      .catchAllCause(c => ZIO.logInfoCause("Failed to list the network interfaces.", c).map (_ => Nil))
    availableInterfaces <- ZIO.filter(interfaces) { i =>
      ZIO.attempt(i.isUp).catchAllCause { c =>
        ZIO.logInfoCause(s"Unable to query the network interface: ${i.getName}.", c).map(_ => false)
      }
    }
    interfaceAddresses = availableInterfaces.flatMap(_.getInterfaceAddresses.asScala)
    broadcastAddresses <- interfaceAddresses.flatMap(Option apply _.getBroadcast) match
      case Nil => DefaultBroadcastAddresses
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
