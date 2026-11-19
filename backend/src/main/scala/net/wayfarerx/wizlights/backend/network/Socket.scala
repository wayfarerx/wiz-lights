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

import java.net.{InetAddress, InetSocketAddress}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import io.circe.parser.parse as parseJson
import io.circe.syntax.*
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.*
import io.netty.channel.socket.DatagramPacket
import io.netty.channel.socket.nio.NioDatagramChannel
import zio.{Chunk, Hub, RIO, Scope, UIO, ZIO}
import zio.stream.ZStream
import protocol.{Request, Response}

/**
 * A socket connected to a network port on all available interfaces.
 *
 * @param port    The network port to connect to.
 * @param channel The active network channel.
 */
private[network] final class Socket(port: Int, channel: Channel):

  /**
   * Sends a request to the specified addresses.
   *
   * @param addresses The addresses to send the request to.
   * @param request   The request to send.
   */
  def apply(addresses: Iterable[InetAddress], request: Request): UIO[Unit] =
    ZIO.suspendSucceed {
      val datagram = request.asJson.noSpaces
      ZIO.foreachDiscard(addresses) { address =>
        ZIO.async[Any, Throwable, Unit] { cb =>
          val packet = DatagramPacket(
            Unpooled.copiedBuffer(datagram, StandardCharsets.UTF_8),
            InetSocketAddress(address, port)
          )
          channel.writeAndFlush(packet).addListener { (future: ChannelFuture) =>
            if future.isSuccess then cb(ZIO.unit)
            else cb(ZIO.fail(Option(future.cause).getOrElse(RuntimeException("Channel write failed"))))
          }
        }.catchAllCause(ZIO.logWarningCause(_))
      }
    }

/**
 * Factory for network sockets.
 */
private[network] object Socket:

  /**
   * Creates a new network socket.
   *
   * @param port           The network port to connect to.
   * @param eventLoopGroup The event loop group to use.
   * @param received       The hub to publish to when an incoming message is received.
   * @return A new network socket.
   */
  def make(port: Int, eventLoopGroup: EventLoopGroup, received: Hub[Incoming]): RIO[Scope, Socket] =
    val handler = Handler()
    for
      _ <- handler.stream.mapZIO(read).collectSome.foreach(received.publish).forkScoped
      channel <- ZIO.acquireRelease(
        ZIO.attemptBlocking(
          Bootstrap()
            .group(eventLoopGroup)
            .channel(classOf[NioDatagramChannel])
            .option[java.lang.Boolean](ChannelOption.SO_BROADCAST, true)
            .option[java.lang.Boolean](ChannelOption.SO_REUSEADDR, true)
            .handler(handler)
            .bind(port)
            .sync()
            .channel()
        )
      )(channel => ZIO.attemptBlocking(channel.close.await).ignore)
    yield Socket(port, channel)

  /**
   * Reads a message from the channel.
   *
   * @param event The address and payload of the received packet.
   * @return A new message if one can be read.
   */
  private[network] def read(event: (InetAddress, ByteBuffer)): UIO[Option[Incoming]] = {
    val (address, payload) = event
    for
      text <- ZIO.attempt(StandardCharsets.UTF_8.decode(payload).toString)
      json <- ZIO.fromEither(parseJson(text))
      response <- ZIO.fromEither(json.as[Response])
    yield Some(Incoming(address, response))
  }.catchAllCause(ZIO.logInfoCause(s"Failed to read an incoming message.", _).map(_ => None))

  /**
   * An inbound UDP packet handler that can target an optional live discovery service.
   */
  private[network] final class Handler extends SimpleChannelInboundHandler[DatagramPacket]:

    /** The callback to emit to the stream. */
    @volatile private var callback: Option[ZStream.Emit[Any, Throwable, (InetAddress, ByteBuffer), Unit]] = None

    /** The stream of packets received by this handler. */
    val stream: ZStream[Any, Throwable, (InetAddress, ByteBuffer)] = ZStream.async(emit => callback = Some(emit))

    /* Forward the datagram to the registered callback. */
    override def channelRead0(context: ChannelHandlerContext, packet: DatagramPacket): Unit = callback.foreach {
      val buffer = ByteBuffer.allocate(packet.content.readableBytes)
      packet.content.readBytes(buffer)
      buffer.flip()
      _(ZIO.succeed(Chunk(packet.sender.getAddress -> buffer)))
    }

    /* Forward the closure to the registered callback. */
    override def channelInactive(context: ChannelHandlerContext): Unit =
      callback.foreach(_(ZIO.fail(None)))
