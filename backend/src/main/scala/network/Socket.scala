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

import java.net.{InetAddress, InetSocketAddress, NetworkInterface}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import scala.collection.immutable.SortedSet
import scala.jdk.CollectionConverters.*

import cats.data.NonEmptySet
import io.circe.parser.parse as parseJson
import io.circe.syntax.*
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.*
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.DatagramPacket
import io.netty.channel.socket.nio.NioDatagramChannel
import zio.stream.{UStream, ZStream}
import zio.{Cause, Chunk, Duration, Hub, Queue, RLayer, Ref, Schedule, Scope, Task, UIO, URIO, ZIO, ZLayer}
import protocol.*

/**
 * A socket for sending and receiving JSON over UDP.
 */
trait Socket:

  /**
   * Publishes a message on this socket.
   *
   * @param message The outgoing message to publish.
   */
  def publish(message: Outgoing): UIO[Unit]

  /**
   * Subscribes to the messages sent to this socket.
   *
   * @return A subscription to the messages sent to this socket.
   */
  def subscribe: URIO[Scope, UStream[Incoming]]

/**
 * A live socket implementation on top of Netty.
 *
 * @param publisher     The queue to offer published messages to.
 * @param subscriptions The hub to get received messages from.
 */
case class SocketLive private(publisher: Queue[Outgoing], subscriptions: Hub[Incoming]) extends Socket:

  /* Publish a message on this socket. */
  override def publish(message: Outgoing): UIO[Unit] =
    publisher.offer(message).unit

  /* Subscribe to the messages sent to this socket. */
  override def subscribe: URIO[Scope, UStream[Incoming]] =
    ZStream.fromHubScoped(subscriptions)

/**
 * Factory for live socket instances.
 */
object SocketLive:

  /** The default broadcast address. */
  private val DefaultBroadcastAddress = ZIO.attempt(InetAddress.ofLiteral("255.255.255.255")).orDie.memoize.flatten

  /** The layer that provides the socket service. */
  val layer: RLayer[Configuration, Socket] = ZLayer.scoped {
    for
      scope <- ZIO.service[Scope]
      config <- ZIO.service[Configuration]
      publisher <- Queue.unbounded[Outgoing]
      subscriptions <- Hub.unbounded[Incoming]
      broadcastAddresses <- DefaultBroadcastAddress.flatMap(Ref make Set(_))
      _ <- refreshBroadcastAddresses(broadcastAddresses)
        .repeat(Schedule.fixed(Duration.fromScala(config.discoveryInterval * Math.E))).forkIn(scope)
      eventLoopGroup <- ZIO.acquireRelease(acquireEventLoopGroup)(releaseEventLoopGroup)
      handler = Handler()
      channel <- ZIO.acquireRelease(acquireChannel(eventLoopGroup, handler))(releaseChannel)
      _ <- handler.stream.mapZIO(read).collectSome.foreach(subscriptions.publish).forkIn(scope)
      _ <- ZStream.fromQueue(publisher).foreach(write(broadcastAddresses, channel, config.networkPort, _)).forkIn(scope)
    yield SocketLive(publisher, subscriptions)
  }

  /**
   * Refreshes the specified set of broadcast addresses.
   *
   * @param broadcastAddressesRef The reference to the current set of broadcast addresses.
   */
  private def refreshBroadcastAddresses(broadcastAddressesRef: Ref[Set[InetAddress]]): UIO[Unit] = for
    interfaces <- ZIO.attempt(Option(NetworkInterface.getNetworkInterfaces).fold(Nil)(_.asScala.toList.filter(_.isUp))) // FIXME
      .catchAllCause(c => ZIO.logInfoCause("Failed to list the network interfaces.", c).map (_ => Nil))
    interfaceAddresses = interfaces.flatMap(_.getInterfaceAddresses.asScala)
    broadcastAddresses <- DefaultBroadcastAddress.map(interfaceAddresses.flatMap(Option apply _.getBroadcast).toSet + _)
    _ <- broadcastAddressesRef.set(broadcastAddresses)
  yield ()

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
   * Acquires a channel using the specified event loop group and handler.
   *
   * @param eventLoopGroup The event loop group to build from.
   * @param handler        The handler to receive incoming events.
   * @return A channel using the specified handler.
   */
  private def acquireChannel(eventLoopGroup: EventLoopGroup, handler: Handler): Task[Channel] =
    ZIO.attemptBlocking(
      Bootstrap()
        .group(eventLoopGroup)
        .channel(classOf[NioDatagramChannel])
        .option[java.lang.Boolean](ChannelOption.SO_BROADCAST, true)
        .handler(handler)
        .bind(0)
        .sync
        .channel
    )

  /**
   * Releases a previously acquired channel.
   *
   * @param channel The channel to release.
   */
  private def releaseChannel(channel: Channel): UIO[Unit] =
    ZIO.attemptBlocking(channel.close.await).ignore

  /**
   * Reads a message from the channel.
   *
   * @param packet Either an error that was received or packet data.
   * @return A new message if one can be read.
   */
  private def read(packet: Either[Throwable, (InetAddress, ByteBuffer)]): UIO[Option[Incoming]] = packet match
    case Left(thrown) =>
      ZIO.logInfoCause(s"Error while reading UDP packets: ${thrown.getMessage}", Cause.die(thrown)).map(_ => None)
    case Right((address, payload)) => {
      for
        text <- ZIO.attempt(StandardCharsets.UTF_8.decode(payload).toString)
        json <- ZIO.fromEither(parseJson(text))
        response <- ZIO.fromEither(json.as[Response])
      yield Some(Incoming(address, response))
    }.catchAllCause(ZIO.logInfoCause(_).map(_ => None))

  /**
   * Writes a message to a channel.
   *
   * @param broadcastAddresses The broadcast addresses available to this service.
   * @param channel            The channel to write to.
   * @param port               The port to write the message on.
   * @param message            The message to write.
   */
  private def write(
    broadcastAddresses: Ref[Set[InetAddress]],
    channel: Channel,
    port: Int,
    message: Outgoing
  ): UIO[Unit] = for
    addresses <- message match
      case Outgoing.Broadcast(_) => broadcastAddresses.get
      case Outgoing.Send(addresses, _) => ZIO.succeed(addresses)
    payload = message.request.asJson.noSpaces
    _ <- ZIO.foreachDiscard(addresses) { address =>
      ZIO.attemptBlocking {
        channel.writeAndFlush(
          DatagramPacket(
            Unpooled.copiedBuffer(payload, StandardCharsets.UTF_8),
            InetSocketAddress(address, port)
          )
        ).sync()
      }.catchAllCause(ZIO.logWarningCause(_)).fork
    }
  yield ()

  /**
   * An inbound UDP packet handler that can target an optional live discovery service.
   */
  private final class Handler extends SimpleChannelInboundHandler[DatagramPacket]:

    /** The callback to emit to the stream. */
    @volatile private var callback: Option[ZStream.Emit[Any, Throwable, Either[Throwable, (InetAddress, ByteBuffer)], Unit]] = None

    /** The stream of packets received by this handler. */
    lazy val stream: ZStream[Any, Throwable, Either[Throwable, (InetAddress, ByteBuffer)]] = ZStream.async(cb => callback = Some(cb))

    /* Forward the datagram to the registered callback. */
    override def channelRead0(ctx: ChannelHandlerContext, packet: DatagramPacket): Unit = callback.foreach {
      val buffer = ByteBuffer.allocate(packet.content.readableBytes)
      packet.content.readBytes(buffer)
      buffer.flip()
      _(ZIO.succeed(Chunk(Right(packet.sender.getAddress -> buffer))))
    }

    /* Forward the cause to the registered callback. */
    @annotation.nowarn("cat=deprecation")
    //noinspection ScalaDeprecation
    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      callback.foreach(_(ZIO.succeed(Chunk(Left(cause)))))

    /* Forward the closure to the registered callback. */
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      callback.foreach(_(ZIO.fail(None)))
