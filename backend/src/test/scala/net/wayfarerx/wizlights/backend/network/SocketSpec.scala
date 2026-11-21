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

import java.net.{DatagramSocket, InetAddress, InetSocketAddress}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference

import scala.collection.mutable.ListBuffer
import scala.util.Using

import io.circe.syntax.*

import io.netty.buffer.Unpooled
import io.netty.channel.{
  ChannelHandlerContext,
  ChannelOutboundHandlerAdapter,
  ChannelPromise,
  EventLoopGroup,
  MultiThreadIoEventLoopGroup
}
import io.netty.channel.embedded.EmbeddedChannel
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.DatagramPacket
import io.netty.util.ReferenceCountUtil

import zio.{durationInt, Chunk, Fiber, Hub, LogLevel, RIO, Scope, Task, UIO, ZIO}
import zio.stream.ZStream
import zio.test.*

import protocol.{GetPilotRequest, RegistrationResponse, RegistrationResponsePayload}

/**
 * Specifications for network sockets.
 */
object SocketSpec extends ZIOSpecDefault:

  /** The port used by sockets over embedded channels. */
  val Port: Int = 38899

  /** A device address. */
  val Device: InetAddress = InetAddress.ofLiteral("192.168.1.50")

  /** Another device address. */
  val OtherDevice: InetAddress = InetAddress.ofLiteral("192.168.1.60")

  /** The text of a valid registration response. */
  val RegistrationText: String = """{"method":"registration","env":"pro","result":{"success":true}}"""

  /** The expected registration response. */
  val Registration: RegistrationResponse = RegistrationResponse(Right(RegistrationResponsePayload(true)))

  /** Creates a scoped event loop group. */
  val eventLoopGroup: RIO[Scope, EventLoopGroup] =
    ZIO.acquireRelease(ZIO.attemptBlocking(MultiThreadIoEventLoopGroup(NioIoHandler.newFactory)))(group =>
      ZIO.attemptBlocking(group.close()).ignore
    )

  /** Returns distinct network ports that were free when inspected. */
  def freePorts(count: Int): Task[List[Int]] =
    ZIO.attemptBlocking {
      val sockets = List.fill(count)(DatagramSocket(0))
      try sockets.map(_.getLocalPort) finally sockets.foreach(_.close())
    }

  /**
   * Sends text to a local network port.
   *
   * @param port The port to send to.
   * @param text The text to send.
   */
  def sendText(port: Int, text: String): Task[Unit] =
    ZIO.attemptBlocking {
      Using.resource(DatagramSocket()) { socket =>
        val bytes = text.getBytes(StandardCharsets.UTF_8)
        val address = InetSocketAddress(InetAddress.getLoopbackAddress, port)
        socket.send(java.net.DatagramPacket(bytes, bytes.length, address))
      }
    }

  /**
   * Reads all the outbound packets written to an embedded channel.
   *
   * @param channel The embedded channel to read from.
   * @return The recipients and text of the outbound packets.
   */
  def outbound(channel: EmbeddedChannel): List[(InetSocketAddress, String)] =
    Iterator.continually(channel.readOutbound[DatagramPacket]()).takeWhile(_ != null).map { packet =>
      try packet.recipient -> packet.content.toString(StandardCharsets.UTF_8) finally packet.release()
    }.toList

  /** Returns the messages logged at the specified level. */
  def logged(level: LogLevel): UIO[Chunk[String]] =
    ZTestLogger.logOutput.map(_.filter(_.logLevel == level).map(_.message()))

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("SocketSpec")(
    suite("apply")(
      test("send the encoded request to every address on the socket's port") {
        val channel = EmbeddedChannel()
        for
          _ <- Socket(Port, channel)(List(Device, OtherDevice), GetPilotRequest(Some(7)))
          warnings <- logged(LogLevel.Warning)
        yield assertTrue(
          outbound(channel) == List(
            InetSocketAddress(Device, Port) -> GetPilotRequest(Some(7)).asJson.noSpaces,
            InetSocketAddress(OtherDevice, Port) -> GetPilotRequest(Some(7)).asJson.noSpaces
          ),
          warnings.isEmpty
        )
      },
      test("suspend until a pending write completes") {
        val pending = AtomicReference[ChannelPromise]()
        val channel = EmbeddedChannel(new ChannelOutboundHandlerAdapter:
          override def write(context: ChannelHandlerContext, message: Any, promise: ChannelPromise): Unit =
            ReferenceCountUtil.release(message)
            pending.set(promise)
        )
        for
          fiber <- Socket(Port, channel)(List(Device), GetPilotRequest()).fork
          status <- (ZIO.yieldNow *> fiber.status).repeatUntil {
            case _: Fiber.Status.Suspended => pending.get != null
            case _ => false
          }
          _ <- ZIO.succeed(pending.get.setSuccess())
          _ <- fiber.join
          warnings <- logged(LogLevel.Warning)
        yield assertTrue(status.isInstanceOf[Fiber.Status.Suspended], warnings.isEmpty)
      },
      test("log failed writes and continue") {
        val channel = EmbeddedChannel()
        for
          _ <- ZIO.attemptBlocking(channel.close().sync())
          _ <- Socket(Port, channel)(List(Device, OtherDevice), GetPilotRequest())
          warnings <- logged(LogLevel.Warning)
        yield assertTrue(warnings.size == 2)
      }
    ),
    suite("read")(
      test("read a valid response") {
        for
          incoming <- Socket.read(Device, RegistrationText)
        yield assertTrue(incoming.contains(Incoming(Device, Registration)))
      },
      test("log and discard an invalid response") {
        for
          incoming <- Socket.read(Device, "not json")
          infos <- logged(LogLevel.Info)
        yield assertTrue(incoming.isEmpty, infos == Chunk("Failed to read an incoming message."))
      }
    ),
    suite("Handler")(
      test("emit the message read from a received packet") {
        val emitted = ListBuffer.empty[ZIO[Any, Option[Throwable], Chunk[Incoming]]]
        val channel = EmbeddedChannel(Socket.Handler(emitted += _))
        channel.writeInbound(DatagramPacket(
          Unpooled.copiedBuffer(RegistrationText, StandardCharsets.UTF_8),
          InetSocketAddress(InetAddress.getLoopbackAddress, Port),
          InetSocketAddress(Device, Port)
        ))
        for
          messages <- ZIO.foreach(emitted.toList)(_.mapError(_.get))
        yield assertTrue(messages == List(Chunk(Incoming(Device, Registration))))
      }
    ),
    suite("make")(
      test("bind a socket that publishes received messages") {
        for
          group <- eventLoopGroup
          hub <- Hub.sliding[Incoming](16)
          port <- freePorts(1).map(_.head)
          _ <- Socket.make(port, group, hub)
          incoming <- ZStream.fromHubScoped(hub)
          _ <- sendText(port, "not json")
          _ <- sendText(port, RegistrationText)
          received <- incoming.runHead
        yield assertTrue(received.contains(Incoming(InetAddress.getLoopbackAddress, Registration)))
      },
      test("fail when the socket cannot be bound") {
        for
          group <- eventLoopGroup
          hub <- Hub.sliding[Incoming](16)
          result <- Socket.make(70000, group, hub).exit
        yield assertTrue(result.isFailure)
      }
    )
  ) @@ TestAspect.timeout(30.seconds)
