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

import zio.{Cause, Chunk, RIO, Scope, Task, UIO, ZIO}
import zio.stream.ZStream

import protocol.{Request, Response}

/**
 * A socket connected to a network port on all available interfaces.
 *
 * @param port    The network port to connect to.
 * @param channel The active network channel.
 */
private final class Socket private(port: Int, channel: Channel):

  /**
   * Sends a request to the specified addresses.
   *
   * @param addresses The addresses to send the request to.
   * @param request   The request to send.
   */
  def apply(addresses: Iterable[InetAddress], request: Request): UIO[Unit] =
    val datagram = request.asJson.noSpaces
    ZIO.foreachParDiscard(addresses) { address =>
      ZIO.attemptBlocking {
        channel.writeAndFlush(
          DatagramPacket(
            Unpooled.copiedBuffer(datagram, StandardCharsets.UTF_8),
            InetSocketAddress(address, port)
          )
        ).sync()
      }.catchAllCause(ZIO.logWarningCause(_))
    }.fork.ignore

/**
 * Factory for network sockets.
 */
private object Socket:

  /** An event emitted by the underlying network stack. */
  private type Event = Either[Throwable, (InetAddress, ByteBuffer)]

  /**
   * Creates a new network socket.
   *
   * @param port           The network port to connect to.
   * @param eventLoopGroup The event loop group to use.
   * @param received       The function to call when an incoming message is received.
   * @return A new network socket.
   */
  def make(port: Int, eventLoopGroup: EventLoopGroup, received: Incoming => UIO[Unit]): RIO[Scope, Socket] =
    val handler = Handler()
    for
      _ <- handler.stream.mapZIO(read).collectSome.foreach(received).forkScoped
      channel <- ZIO.acquireRelease(acquireChannel(port, eventLoopGroup, handler))(releaseChannel)
    yield Socket(port, channel)

  /**
   * Acquires a channel using the specified port, event loop group, and handler.
   *
   * @param port           The port number to bind to.
   * @param eventLoopGroup The event loop group to build from.
   * @param handler        The handler to dispatch to.
   * @return A channel using the specified handler.
   */
  private def acquireChannel(port: Int, eventLoopGroup: EventLoopGroup, handler: Handler): Task[Channel] =
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

  /**
   * Releases a previously acquired channel.
   *
   * @param channel The channel to release.
   * @return An effect that releases the channel.
   */
  private def releaseChannel(channel: Channel): UIO[Unit] =
    ZIO.attemptBlocking(channel.close.await).ignore

  /**
   * Reads a message from the channel.
   *
   * @param event Either an error that was received or packet data.
   * @return A new message if one can be read.
   */
  private def read(event: Event): UIO[Option[Incoming]] = event match
    case Left(thrown) =>
      ZIO.logInfoCause(s"Error while reading UDP packet: ${thrown.getMessage}", Cause.die(thrown)).map(_ => None)
    case Right((address, payload)) => {
      for
        text <- ZIO.attempt(StandardCharsets.UTF_8.decode(payload).toString)
        json <- ZIO.fromEither(parseJson(text))
        response <- ZIO.fromEither(json.as[Response])
      yield Some(Incoming(address, response))
    }.catchAllCause(ZIO.logInfoCause(_).map(_ => None))

  /**
   * An inbound UDP packet handler that can target an optional live discovery service.
   */
  private final class Handler extends SimpleChannelInboundHandler[DatagramPacket]:

    /** The callback to emit to the stream. */
    @volatile private var callback: Option[ZStream.Emit[Any, Throwable, Event, Unit]] = None

    /** The stream of packets received by this handler. */
    val stream: ZStream[Any, Throwable, Event] = ZStream.async(emit => callback = Some(emit))

    /* Forward the datagram to the registered callback. */
    override def channelRead0(context: ChannelHandlerContext, packet: DatagramPacket): Unit = callback.foreach {
      val buffer = ByteBuffer.allocate(packet.content.readableBytes)
      packet.content.readBytes(buffer)
      buffer.flip()
      _(ZIO.succeed(Chunk(Right(packet.sender.getAddress -> buffer))))
    }

    /* Forward the cause to the registered callback. */
    @annotation.nowarn("cat=deprecation")
    //noinspection ScalaDeprecation
    override def exceptionCaught(context: ChannelHandlerContext, cause: Throwable): Unit =
      callback.foreach(_(ZIO.succeed(Chunk(Left(cause)))))

    /* Forward the closure to the registered callback. */
    override def channelInactive(context: ChannelHandlerContext): Unit =
      callback.foreach(_(ZIO.fail(None)))
