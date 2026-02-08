package net.wayfarerx.wizlights
package backend
package network

import io.circe.parser.parse as parseJson
import io.circe.syntax.*
import io.netty.bootstrap.Bootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.*
import io.netty.channel.nio.NioIoHandler
import io.netty.channel.socket.DatagramPacket
import io.netty.channel.socket.nio.NioDatagramChannel
import protocol.*
import zio.stream.{UStream, ZStream}
import zio.{Chunk, Hub, Queue, RLayer, Scope, Task, UIO, URIO, ZIO, ZLayer}

import java.net.{InetAddress, InetSocketAddress, NetworkInterface}
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

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
 * @param publisher          The queue to offer published messages to.
 * @param subscriptions      The hub to get received messages from.
 * @param broadcastAddresses The set of available broadcast addresses.
 */
private final class SocketLive private(
  publisher: Queue[Outgoing.Unicast],
  subscriptions: Hub[Incoming],
  broadcastAddresses: Set[InetAddress]
) extends Socket:

  /* Publish a message on this socket. */
  override def publish(message: Outgoing): UIO[Unit] = message match
    case Outgoing.Broadcast(request) =>
      ZIO.foreachDiscard(broadcastAddresses)(address => publisher.offer(Outgoing.Unicast(address, request)))
    case Outgoing.Multicast(addresses, request) =>
      ZIO.foreachDiscard(addresses.toSortedSet)(address => publisher.offer(Outgoing.Unicast(address, request)))
    case message: Outgoing.Unicast =>
      publisher.offer(message).unit

  /* Subscribe to the messages sent to this socket. */
  override def subscribe: URIO[Scope, UStream[Incoming]] =
    ZStream.fromHubScoped(subscriptions)

/**
 * Factory for live socket instances.
 */
object SocketLive:

  /** The layer that provides the socket service. */
  val layer: RLayer[Configuration, Socket] = ZLayer.scoped {
    for
      config <- ZIO.service[Configuration]
      scope <- ZIO.service[Scope]
      publisher <- Queue.unbounded[Outgoing.Unicast]
      subscriptions <- Hub.unbounded[Incoming]
      broadcastAddresses <- ZIO.attemptBlocking {
        for
          interface <- NetworkInterface.getNetworkInterfaces.asScala
          interfaceAddress <- interface.getInterfaceAddresses.asScala
          broadcastAddress <- Option(interfaceAddress.getBroadcast)
        yield broadcastAddress
      }.map(_.toSet)
      eventLoopGroup <- ZIO.acquireRelease(acquireEventLoopGroup)(releaseEventLoopGroup)
      handler = Handler()
      channel <- ZIO.acquireRelease(acquireChannel(eventLoopGroup, handler))(releaseChannel)
      _ <- handler.stream.mapZIO(read.tupled).collectSome.foreach(subscriptions.publish).forkIn(scope)
      _ <- ZStream.fromQueue(publisher).foreach(write(channel, config.networkPort, _)).forkIn(scope)
    yield SocketLive(publisher, subscriptions, broadcastAddresses)
  }

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
   * @param channel The channel to release,
   */
  private def releaseChannel(channel: Channel): UIO[Unit] =
    ZIO.attemptBlocking(channel.close.await).ignore

  /**
   * Reads a message from the channel.
   *
   * @param address The address of the sender.
   * @param payload The content of the message.
   * @return A new message if one can be read.
   */
  private def read(address: InetAddress, payload: ByteBuffer): UIO[Option[Incoming]] = {
    for {
      text <- ZIO.attempt(StandardCharsets.UTF_8.decode(payload).toString)
      json <- ZIO.fromEither(parseJson(text))
      response <- ZIO.fromEither(json.as[Response])
    } yield Some(Incoming(address, response))
  }.catchAllCause(ZIO.logInfoCause(_).map(_ => None))

  /**
   * Writes a message to a channel.
   *
   * @param channel The channel to write to.
   * @param port    The port to write the message on.
   * @param message The message to write.
   */
  private def write(channel: Channel, port: Int, message: Outgoing.Unicast): UIO[Unit] =
    ZIO.attemptBlocking {
      channel.writeAndFlush(
        DatagramPacket(
          Unpooled.copiedBuffer(message.request.asJson.noSpaces, StandardCharsets.UTF_8),
          InetSocketAddress(message.address, port)
        )
      ).sync()
    }.unit.catchAllCause(ZIO.logWarningCause(_))

  /**
   * An inbound UDP packet handler that can target an optional live discovery service.
   */
  private final class Handler extends SimpleChannelInboundHandler[DatagramPacket]:

    /** The callback to emit to the stream. */
    @volatile private var callback: Option[ZStream.Emit[Any, Throwable, (InetAddress, ByteBuffer), Unit]] = None

    /** The stream of packets received by this handler. */
    lazy val stream: ZStream[Any, Throwable, (InetAddress, ByteBuffer)] = ZStream.async(cb => callback = Some(cb))

    /* Forward the datagram to the registered callback. */
    override def channelRead0(ctx: ChannelHandlerContext, packet: DatagramPacket): Unit = callback.foreach {
      val buffer = ByteBuffer.allocate(packet.content.readableBytes)
      packet.content.readBytes(buffer)
      buffer.flip()
      _(ZIO.succeed(Chunk(packet.sender.getAddress -> buffer)))
    }

    /* Forward the cause to the registered callback. */
    @annotation.nowarn("cat=deprecation")
    //noinspection ScalaDeprecation
    override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable): Unit =
      callback.foreach(_(ZIO.fail(Some(cause))))

    /* Forward the closure to the registered callback. */
    override def channelInactive(ctx: ChannelHandlerContext): Unit =
      callback.foreach(_(ZIO.fail(None)))
