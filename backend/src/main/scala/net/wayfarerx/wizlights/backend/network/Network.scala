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

import scala.concurrent.duration.*

import io.netty.channel.nio.NioIoHandler
import io.netty.channel.{EventLoopGroup, MultiThreadIoEventLoopGroup}

import zio.{Duration, Hub, Queue, RIO, RLayer, Ref, Schedule, Scope, Task, UIO, URIO, ZIO, ZLayer}
import zio.stream.{UStream, ZStream}

/**
 * A service that manages interaction with the network.
 */
trait Network:

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
   * Subscribes to events describing changes to the local network topology.
   *
   * @return A subscription to topology change events.
   */
  def events: URIO[Scope, UStream[Event]]

/**
 * The live implementation of the network service.
 *
 * @param topologyRef   The cached local network topology.
 * @param outgoingQueue The queue of outgoing messages.
 * @param incomingHub   The hub of incoming messages.
 * @param eventHub      The hub of topology change events.
 */
final class NetworkLive private[network](
  topologyRef: Ref.Synchronized[Topology],
  outgoingQueue: Queue[Outgoing],
  incomingHub: Hub[Incoming],
  eventHub: Hub[Event]
) extends Network:

  /* Return the local MAC address and internet address for communicating with the specified target if available. */
  override def localhost(target: InetAddress): UIO[Option[(String, String)]] =
    topologyRef.get.map(_.interfaceFor(target).map(_.macAndIpAddresses))

  /* Publish a command message on the command port. */
  override def publish(message: Outgoing): UIO[Unit] =
    outgoingQueue.offer(message).ignore

  /* Subscribe to responses received across all response ports. */
  override def subscribe: URIO[Scope, UStream[Incoming]] =
    ZStream.fromHubScoped(incomingHub)

  /* Subscribes to events describing changes to the local network topology. */
  override def events: URIO[Scope, UStream[Event]] =
    ZStream.fromHubScoped(eventHub)

/**
 * Factory for live network services.
 */
object NetworkLive:

  /** The golden ratio for staggering broadcast address refreshes. */
  private[network] val GoldenRatio = (1.0 + Math.sqrt(5.0)) / 2.0

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
   * @param outgoingWindow     The number of outgoing requests to keep in the network queue.
   * @param incomingWindow     The number of incoming responses to keep in the network hub.
   * @param commandPort        The network port to send commands on.
   * @param listenerPort       The network port to also listen on.
   * @param discoveryInterval  The interval to wait between discovering devices.
   * @return A live network representation.
   */
  def make(
    outgoingWindow: Int,
    incomingWindow: Int,
    commandPort: Int,
    listenerPort: Int,
    discoveryInterval: FiniteDuration
  ): RIO[Scope, Network] =
    val refreshTopologyInterval = Duration.fromScala(discoveryInterval * GoldenRatio)
    for
      eventLoopGroup <- ZIO.acquireRelease(acquireEventLoopGroup)(releaseEventLoopGroup)
      outgoingQueue <- Queue.sliding[Outgoing](outgoingWindow)
      incomingHub <- Hub.sliding[Incoming](incomingWindow)
      eventHub <- Hub.sliding[Event](16)
      commandSocket <- Socket.make(commandPort, eventLoopGroup, incomingHub)
      listenerSocket <- Socket.make(listenerPort, eventLoopGroup, incomingHub)
      topologyRef <- Ref.Synchronized.make[Topology](Topology.Empty)
      _ <- refreshTopology(topologyRef, eventHub)
      _ <- refreshTopology(topologyRef, eventHub)
        .repeat(Schedule.spaced(refreshTopologyInterval)).delay(refreshTopologyInterval).forkScoped
      _ <- ZStream.fromQueue(outgoingQueue).foreach(write(topologyRef, commandSocket, _)).forkScoped
    yield NetworkLive(topologyRef, outgoingQueue, incomingHub, eventHub)

  /**
   * Acquires an event loop group.
   *
   * @return The acquired event loop group.
   */
  private[network] def acquireEventLoopGroup: Task[EventLoopGroup] =
    ZIO.attemptBlocking(MultiThreadIoEventLoopGroup(NioIoHandler.newFactory))

  /**
   * Releases an event loop group.
   *
   * @param group The event loop group to release.
   */
  private[network] def releaseEventLoopGroup(group: EventLoopGroup): UIO[Unit] =
    ZIO.attemptBlocking(group.close()).ignore

  /**
   * Refreshes the cached network topology.
   *
   * @param topologyRef The reference to the current network topology.
   * @param eventHub    The hub of topology change events.
   */
  private[network] def refreshTopology(topologyRef: Ref.Synchronized[Topology], eventHub: Hub[Event]): UIO[Unit] =
    for
      newTopology <- Topology.discover().map(Some(_)).catchAllCause {
        ZIO.logWarningCause(s"Failed to discover the local network topology.", _).map(_ => None)
      }
      _ <- newTopology.fold(ZIO.unit) { newTopology =>
        topologyRef.modifyZIO { oldTopology =>
          eventHub.publishAll(oldTopology.diff(newTopology)).map(_ -> newTopology)
        }
      }
    yield ()

  /**
   * Writes a message to the specified socket.
   *
   * @param topologyRef The live network topology.
   * @param socket      The socket to write to.
   * @param message     The message to write to the socket.
   */
  private[network] def write(topologyRef: Ref.Synchronized[Topology], socket: Socket, message: Outgoing): UIO[Unit] =
    topologyRef.get.flatMap(t => socket(t.destinations(message), message.request))

