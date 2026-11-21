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

import java.net.{InetAddress, InetSocketAddress, SocketException}

import scala.concurrent.duration.*

import io.circe.syntax.*

import io.netty.channel.embedded.EmbeddedChannel

import zio.{Chunk, Hub, LogLevel, Queue, Scope, UIO, ZIO, ZLayer}
import zio.stream.SubscriptionRef
import zio.test.*

import protocol.GetPilotRequest

import InterfaceSpec.interface
import SocketSpec.{freePorts, logged, outbound, sendText, Device, Port, Registration, RegistrationText}

/**
 * Specifications for the network service.
 */
object NetworkSpec extends ZIOSpecDefault:

  /** An interface on the 192.168.1.0/24 subnet. */
  val home: Interface = interface("aabbccddeeff", "192.168.1.10", 24, Some("192.168.1.255"))

  /** An interface on the 10.0.0.0/8 subnet. */
  val work: Interface = interface("112233445566", "10.0.0.5", 8, Some("10.255.255.255"))

  /**
   * Creates a network service over an embedded channel.
   *
   * @param topology The initial topology.
   * @return The network service, its topology, its embedded channel and its incoming hub.
   */
  def embedded(topology: Topology): UIO[(NetworkLive, SubscriptionRef[Topology], EmbeddedChannel, Hub[Incoming])] =
    for
      topologyRef <- SubscriptionRef.make(topology)
      channel = EmbeddedChannel()
      hub <- Hub.sliding[Incoming](16)
    yield (NetworkLive(topologyRef, Socket(Port, channel), hub), topologyRef, channel, hub)

  /** An interface used to detect when a subscription to topology events is active. */
  val probe: Interface = interface("665544332211", "172.16.0.1", 12)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("NetworkSpec")(
    suite("NetworkLive")(
      test("events describes the changes to the topology") {
        for
          (network, topologyRef, _, _) <- embedded(Topology(List(home)))
          events <- Queue.unbounded[Event]
          _ <- network.events.foreach(events.offer).forkScoped
          _ <- (topologyRef.set(Topology(List(home, probe))) *> topologyRef.set(Topology(List(home))) *> ZIO.yieldNow)
            .repeatUntilZIO(_ => events.isEmpty.map(!_))
          _ <- topologyRef.set(Topology(List(home, work)))
          _ <- events.take.repeatUntil(_ == Event.Activated(work.macAddress, work.ipAddress, work.prefixLength))
          _ <- topologyRef.set(Topology(List(home.copy(broadcastAddress = None), work)))
          _ <- topologyRef.set(Topology(List(work.copy(prefixLength = 16))))
          changes <- events.takeN(3)
          remaining <- events.size
        yield assertTrue(
          changes == Chunk(
            Event.Deactivated(home.macAddress, home.ipAddress, home.prefixLength),
            Event.Deactivated(work.macAddress, work.ipAddress, work.prefixLength),
            Event.Activated(work.macAddress, work.ipAddress, 16)
          ),
          remaining == 0
        )
      },
      test("localhost returns the local interface for a target") {
        for
          (network, _, _, _) <- embedded(Topology(List(home, work)))
          local <- network.localhost(InetAddress.ofLiteral("192.168.1.50"))
          remote <- network.localhost(InetAddress.ofLiteral("172.16.0.1"))
        yield assertTrue(local.contains("aabbccddeeff" -> "192.168.1.10"), remote.isEmpty)
      },
      test("publish sends messages to the topology's destinations") {
        val request = GetPilotRequest(Some(3))
        for
          (network, _, channel, _) <- embedded(Topology(List(home, work)))
          _ <- network.publish(Outgoing.Broadcast(request))
          broadcast = outbound(channel)
          _ <- network.publish(Outgoing.Send(Set(Device), request))
          sent = outbound(channel)
        yield assertTrue(
          broadcast.toSet == Set(
            InetSocketAddress(InetAddress.ofLiteral("192.168.1.255"), Port) -> request.asJson.noSpaces,
            InetSocketAddress(InetAddress.ofLiteral("10.255.255.255"), Port) -> request.asJson.noSpaces
          ),
          sent == List(InetSocketAddress(Device, Port) -> request.asJson.noSpaces)
        )
      },
      test("subscribe receives incoming messages") {
        for
          (network, _, _, hub) <- embedded(Topology.Empty)
          incoming <- network.subscribe
          _ <- hub.publish(Incoming(Device, Registration))
          received <- incoming.runHead
        yield assertTrue(received.contains(Incoming(Device, Registration)))
      }
    ),
    suite("make")(
      test("discover the topology at startup and then on every refresh interval") {
        for
          ports <- freePorts(2)
          discovered <- Queue.unbounded[Topology]
          _ <- discovered.offerAll(List(Topology(List(home)), Topology(List(work))))
          network <- NetworkLive.make(16, ports.head, ports.last, 1.minute, discovered.take)
          initial <- network.localhost(work.ipAddress)
          _ <- TestClock.adjust(zio.Duration.fromScala(1.minute))
          refreshed <- network.localhost(work.ipAddress).repeatUntil(_.nonEmpty)
          stale <- network.localhost(home.ipAddress)
        yield assertTrue(initial.isEmpty, refreshed.contains(work.macAndIpAddresses), stale.isEmpty)
      },
      test("log failures to discover the topology") {
        for
          ports <- freePorts(2)
          network <- NetworkLive.make(16, ports.head, ports.last, 1.minute, ZIO.fail(SocketException("boom")))
          local <- network.localhost(home.ipAddress)
          warnings <- logged(LogLevel.Warning)
        yield assertTrue(local.isEmpty, warnings.contains("Failed to discover the local network topology."))
      },
      test("receive messages on both the command and listener ports") {
        for
          ports <- freePorts(2)
          network <- NetworkLive.make(16, ports.head, ports.last, 1.minute, ZIO.succeed(Topology.Empty))
          incoming <- network.subscribe
          _ <- sendText(ports.head, RegistrationText)
          _ <- sendText(ports.last, RegistrationText)
          received <- incoming.take(2).runCollect
        yield assertTrue(received == Chunk.fill(2)(Incoming(InetAddress.getLoopbackAddress, Registration)))
      },
      test("layer provides a network service from the configuration") {
        for
          ports <- freePorts(2)
          _ <- ZIO.serviceWithZIO[Network] { network =>
            network.publish(Outgoing.Send(Set(InetAddress.getLoopbackAddress), GetPilotRequest()))
          }.provide(
            ZLayer(Configuration.make(commandPort = ports.head, listenerPort = ports.last)),
            NetworkLive.layer
          )
        yield assertCompletes
      }
    )
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(30))
