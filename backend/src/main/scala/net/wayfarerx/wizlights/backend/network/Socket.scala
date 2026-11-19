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
import java.nio.charset.StandardCharsets

import io.circe.parser.parse as parseJson
import io.circe.syntax.*

import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.*
import io.netty.channel.socket.DatagramPacket
import io.netty.channel.socket.nio.NioDatagramChannel

import zio.{Chunk, Hub, Promise, RIO, Scope, UIO, ZIO}
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
    for
      bound <- Promise.make[Throwable, Channel]
      _ <- ZStream.asyncScoped[Any, Throwable, Incoming] { emit =>
        ZIO.acquireRelease(
          ZIO.attemptBlocking(
            Bootstrap()
              .group(eventLoopGroup)
              .channel(classOf[NioDatagramChannel])
              .option[java.lang.Boolean](ChannelOption.SO_BROADCAST, true)
              .option[java.lang.Boolean](ChannelOption.SO_REUSEADDR, true)
              .handler(Handler(emit))
              .bind(port)
              .sync()
              .channel()
          )
        )(channel => ZIO.attemptBlocking(channel.close.await).ignore).flatMap(bound.succeed).tapErrorCause(bound.failCause)
      }.foreach(received.publish).forkScoped
      channel <- bound.await
    yield Socket(port, channel)

  /**
   * Reads a message from the text of a received packet.
   *
   * @param address The address of the device that sent the packet.
   * @param text    The text of the received packet.
   * @return A new message if one can be read.
   */
  private[network] def read(address: InetAddress, text: String): UIO[Option[Incoming]] = {
    for
      json <- ZIO.fromEither(parseJson(text))
      response <- ZIO.fromEither(json.as[Response])
    yield Some(Incoming(address, response))
  }.catchAllCause(ZIO.logInfoCause(s"Failed to read an incoming message.", _).map(_ => None))

  /**
   * An inbound UDP packet handler that emits the messages it reads to a stream.
   *
   * @param emit The callback that emits received messages to the stream.
   */
  private[network] final class Handler(emit: ZIO[Any, Option[Throwable], Chunk[Incoming]] => Unit)
    extends SimpleChannelInboundHandler[DatagramPacket]:

    /* Read the datagram and emit the resulting message. */
    override def channelRead0(context: ChannelHandlerContext, packet: DatagramPacket): Unit =
      emit(read(packet.sender.getAddress, packet.content.toString(StandardCharsets.UTF_8)).map(Chunk.fromIterable))
