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
package internal

import java.net.{Inet4Address, InetAddress, NetworkInterface}
import java.time.Instant

import scala.annotation.targetName
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import zio.stream.{UStream, ZStream}
import zio.{Clock, Duration, Hub, Ref, RLayer, Schedule, Scope, UIO, URIO, ZIO, ZLayer}

import model.*
import network.{Incoming, Network, Outgoing}
import protocol.*
import service.Lighting

/**
 * The live implementation of the lighting service.
 *
 * @param configuration        The backend configuration.
 * @param network              The network service.
 * @param hub                  The hub of light events.
 * @param recordsRef           The reference to known light records.
 * @param responseListenersRef The reference to active response listeners for retries.
 */
final class LightingLive private[internal](
  configuration: Configuration,
  network: Network,
  hub: Hub[(Light, Option[Source])],
  recordsRef: Ref[Map[Address, LightingLive.Record]],
  responseListenersRef: Ref[Set[InetAddress => UIO[Unit]]]
) extends Lighting:

  /* Return the lights managed by this service. */
  override def lights: UIO[Set[Light]] =
    recordsRef.get.map(_.values.map(_.toLight).toSet)

  /* Returns the light with the specified address. */
  override def get(address: Address): UIO[Light] =
    recordsRef.get.map(_.get(address).map(_.toLight).getOrElse(Light(address, None, None)))

  /* Returns the lights with the specified addresses. */
  override def getAll(addresses: Iterable[Address]): UIO[Set[Light]] =
    recordsRef.get.map { map =>
      addresses.map(a => map.get(a).map(_.toLight).getOrElse(Light(a, None, None))).toSet
    }

  /* Update the enabled state of the light at the specified address. */
  override def update(address: Address, enabled: Boolean): UIO[Boolean] =
    updateAll(List(address), enabled).map(_ > 0)

  /* Update the enabled state of the lights at the specified addresses. */
  override def updateAll(addresses: Iterable[Address], enabled: Boolean): UIO[Int] =
    executeUpdate(
      addresses,
      SetPilotRequest(state = Some(enabled)),
      current =>
        if enabled then
          current match
            case Some(Status.Disabled) => Some(Status.White(Temperature.Medium, Brightness.Brightest))
            case Some(other) => Some(other)
            case None => Some(Status.White(Temperature.Medium, Brightness.Brightest))
        else Some(Status.Disabled)
    )

  /* Update the brightness of the light at the specified address. */
  @targetName("updateBrightness")
  override def update(address: Address, brightness: Brightness): UIO[Boolean] =
    updateAll(List(address), brightness).map(_ > 0)

  /* Update the brightness of the lights at the specified addresses. */
  @targetName("updateAllBrightness")
  override def updateAll(addresses: Iterable[Address], brightness: Brightness): UIO[Int] =
    executeUpdate(
      addresses,
      SetPilotRequest(dimming = Some(brightness: Int)),
      {
        case Some(Status.White(t, _)) => Some(Status.White(t, brightness))
        case Some(Status.Defined.Static(s, _)) => Some(Status.Defined.Static(s, brightness))
        case Some(Status.Defined.Dynamic(s, sp, _)) => Some(Status.Defined.Dynamic(s, sp, brightness))
        case Some(Status.Custom(c, _)) => Some(Status.Custom(c, brightness))
        case Some(Status.Disabled) => Some(Status.White(Temperature.Medium, brightness))
        case None => Some(Status.White(Temperature.Medium, brightness))
      }
    )

  /* Update the color temperature of the light at the specified address. */
  @targetName("updateTemperature")
  override def update(address: Address, temperature: Temperature): UIO[Boolean] =
    updateAll(List(address), temperature).map(_ > 0)

  /* Update the color temperature of the lights at the specified addresses. */
  @targetName("updateAllTemperature")
  override def updateAll(addresses: Iterable[Address], temperature: Temperature): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), temp = Some(temperature: Int))
    executeUpdate(
      addresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.White(temperature, b))
    )

  /* Update the color of the light at the specified address. */
  override def update(address: Address, color: Color): UIO[Boolean] =
    updateAll(List(address), color).map(_ > 0)

  /* Update the color of the lights at the specified addresses. */
  override def updateAll(addresses: Iterable[Address], color: Color): UIO[Int] =
    val request = SetPilotRequest(
      state = Some(true),
      r = Some(color.red),
      g = Some(color.green),
      b = Some(color.blue),
      c = Some(color.cool),
      w = Some(color.warm)
    )
    executeUpdate(
      addresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.Custom(color, b))
    )

  /* Update the scene of the light at the specified address. */
  override def update(address: Address, scene: Scene): UIO[Boolean] =
    updateAll(List(address), scene).map(_ > 0)

  /* Update the scene of the lights at the specified addresses. */
  override def updateAll(addresses: Iterable[Address], scene: Scene): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), sceneId = Some(scene.id))
    executeUpdate(
      addresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        if scene.isDynamic then
          val sp = current.collect { case d: Status.Defined.Dynamic => d.speed }.getOrElse(Speed.Medium)
          Some(Status.Defined.Dynamic(scene.asInstanceOf[Scene.Dynamic], sp, b))
        else
          Some(Status.Defined.Static(scene.asInstanceOf[Scene.Static], b))
    )

  /* Update the dynamic scene and speed of the light at the specified address. */
  override def update(address: Address, scene: Scene.Dynamic, speed: Speed): UIO[Boolean] =
    updateAll(List(address), scene, speed).map(_ > 0)

  /* Update the dynamic scene and speed of the lights at the specified addresses. */
  override def updateAll(addresses: Iterable[Address], scene: Scene.Dynamic, speed: Speed): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed: Int))
    executeUpdate(
      addresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.Defined.Dynamic(scene, speed, b))
    )

  /* Update the dynamic scene speed of the light at the specified address. */
  @targetName("updateSpeed")
  override def update(address: Address, speed: Speed): UIO[Boolean] =
    updateAll(List(address), speed).map(_ > 0)

  /* Update the dynamic scene speed of the lights at the specified addresses. */
  @targetName("updateAllSpeed")
  override def updateAll(addresses: Iterable[Address], speed: Speed): UIO[Int] =
    executeUpdate(
      addresses,
      SetPilotRequest(speed = Some(speed: Int)),
      {
        case Some(Status.Defined.Dynamic(s, _, b)) => Some(Status.Defined.Dynamic(s, speed, b))
        case other => other
      }
    )

  /* Update the status of the light at the specified address. */
  override def update(address: Address, status: Status): UIO[Boolean] =
    updateAll(List(address), status).map(_ > 0)

  /* Update the statuses of the lights at the specified addresses. */
  override def updateAll(addresses: Iterable[Address], status: Status): UIO[Int] =
    executeUpdate(
      addresses,
      statusToSetPilotRequest(status),
      _ => Some(status)
    )

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[(Light, Option[Source])]] =
    ZStream.fromHubScoped(hub)

  /** Broadcasts discovery requests on the network. */
  def discover: UIO[Unit] = for
    _ <- network.publish(Outgoing.Broadcast(GetSystemConfigRequest))
    _ <- network.publish(Outgoing.Broadcast(GetPilotRequest))
    credentials <- localCredentials
    _ <- credentials match
      case Some((mac, ip)) => network.publish(Outgoing.Broadcast(RegistrationRequest(mac, ip)))
      case None => ZIO.unit
  yield ()

  /**
   * Sends an outgoing command with retries that track responding devices.
   *
   * @param targets The target IP addresses.
   * @param request The request to send.
   */
  private def sendCommand(targets: Set[InetAddress], request: Request): UIO[Unit] =
    if targets.isEmpty then ZIO.unit
    else if configuration.retryBackoffs.isEmpty then
      network.publish(Outgoing.Send(targets, request))
    else
      for
        pendingRef <- Ref.make(targets)
        listener = (ip: InetAddress) => pendingRef.update(_ - ip)
        _ <- responseListenersRef.update(_ + listener)
        _ <- network.publish(Outgoing.Send(targets, request))
        _ <- retryLoop(pendingRef, request, configuration.retryBackoffs)
          .ensuring(responseListenersRef.update(_ - listener))
          .fork
      yield ()

  /**
   * Retries sending a request to devices that have not yet responded.
   *
   * @param pendingRef The reference to remaining target addresses.
   * @param request    The request to send.
   * @param backoffs   The remaining backoff durations.
   */
  private def retryLoop(
    pendingRef: Ref[Set[InetAddress]],
    request: Request,
    backoffs: List[FiniteDuration]
  ): UIO[Unit] = backoffs match
    case Nil => ZIO.unit
    case head :: tail =>
      ZIO.sleep(Duration.fromScala(head)) *> pendingRef.get.flatMap { remaining =>
        if remaining.isEmpty then ZIO.unit
        else network.publish(Outgoing.Send(remaining, request)) *> retryLoop(pendingRef, request, tail)
      }

  /**
   * Notifies active retry listeners that a device at the specified IP address has responded.
   *
   * @param senderIp The IP address of the responding device.
   */
  private def notifyResponse(senderIp: InetAddress): UIO[Unit] =
    responseListenersRef.get.flatMap { listeners =>
      if listeners.isEmpty then ZIO.unit
      else ZIO.foreachDiscard(listeners)(_(senderIp))
    }

  /**
   * Executes a partial or full status update on lights matching the specified addresses.
   *
   * @param addresses    The addresses of the lights to update.
   * @param request      The pilot request to send.
   * @param updateStatus The function to update the local cached status.
   * @return The number of lights found and updated.
   */
  private def executeUpdate(
    addresses: Iterable[Address],
    request: SetPilotRequest,
    updateStatus: Option[Status] => Option[Status]
  ): UIO[Int] = for
    targetAddresses <- ZIO.succeed(addresses.toSet)
    currentRecords <- recordsRef.get
    matching = targetAddresses.flatMap { addr =>
      currentRecords.get(addr).flatMap(rec => rec.inetAddress.map(ip => (addr, rec, ip)))
    }
    _ <- if matching.isEmpty then ZIO.unit else
      val ips = matching.map(_._3)
      for
        now <- Clock.instant
        updatedLights <- recordsRef.modify { map =>
          val updated = matching.foldLeft(map) { case (acc, (addr, rec, _)) =>
            val newStatus = updateStatus(rec.status)
            val newRec = rec.copy(status = newStatus, lastSeen = Some(now))
            acc.updated(addr, newRec)
          }
          val lights = matching.flatMap { case (addr, _, _) => updated.get(addr).map(_.toLight) }
          (lights, updated)
        }
        _ <- sendCommand(ips, request)
        _ <- ZIO.foreachDiscard(updatedLights) { light =>
          hub.publish((light, Some(Source.Udp)))
        }
      yield ()
  yield matching.size

  /**
   * Registers this controller with the specified device IP.
   *
   * @param target The device IP address to register with.
   */
  private def registerWith(target: InetAddress): UIO[Unit] =
    localCredentials.flatMap {
      case Some((mac, ip)) => network.publish(Outgoing.Send(Set(target), RegistrationRequest(mac, ip)))
      case None => ZIO.unit
    }

  /** Resolves the local MAC address and IPv4 address. */
  private val localCredentials: UIO[Option[(String, String)]] = ZIO.attempt {
    Option(NetworkInterface.getNetworkInterfaces).flatMap { interfaces =>
      interfaces.asScala.find { i =>
        !i.isLoopback && i.isUp && i.getHardwareAddress != null && i.getHardwareAddress.length == 6
      }.flatMap { i =>
        val mac = i.getHardwareAddress.map("%02x".format(_)).mkString
        i.getInetAddresses.asScala.find {
          case _: Inet4Address => true
          case _ => false
        }.map(ip => (mac, ip.getHostAddress))
      }
    }
  }.catchAll(_ => ZIO.none)

  /**
   * Processes incoming network responses.
   *
   * @param incoming The incoming response.
   */
  private[internal] def handleIncoming(incoming: Incoming): UIO[Unit] =
    notifyResponse(incoming.address) *> (incoming.response match
      case FirstBeatResponse(Right(payload)) =>
        handleFirstBeat(incoming.address, payload)
      case GetSystemConfigResponse(Right(payload)) =>
        handleSystemConfig(incoming.address, payload)
      case GetPilotResponse(Right(payload)) =>
        handleGetPilot(incoming.address, payload)
      case SyncPilotResponse(Right(payload)) =>
        handleSyncPilot(incoming.address, payload)
      case _ =>
        ZIO.unit
    )

  /**
   * Handles a firstBeat response.
   *
   * @param senderIp The IP address of the sender.
   * @param payload  The firstBeat payload.
   */
  private def handleFirstBeat(senderIp: InetAddress, payload: FirstBeatResponsePayload): UIO[Unit] =
    Address.make(payload.mac).foldZIO(
      err => ZIO.logWarning(s"Invalid MAC in firstBeat from $senderIp: ${err.getMessage}"),
      address =>
        for
          homeOpt <- payload.homeId.fold(ZIO.none)(h => Home.make(h).option)
          roomOpt <- payload.roomId.fold(ZIO.none)(r => Room.make(r).option)
          modOpt <- payload.moduleName.fold(ZIO.none)(m => Module.make(m).option)
          fwOpt <- payload.fwVersion.fold(ZIO.none)(f => Firmware.make(f).option)
          devOpt <- payload.typeId.fold(ZIO.none)(d => Device.make(d).option)
          grpOpt <- payload.groupId.fold(ZIO.none)(g => Group.make(g).option)
          now <- Clock.instant
          updatedLight <- recordsRef.modify { map =>
            val existing = map.getOrElse(address, LightingLive.Record(address))
            val updated = existing.copy(
              inetAddress = Some(senderIp),
              signal = existing.signal.orElse(Some(Signal.Medium)),
              home = homeOpt.orElse(existing.home),
              room = roomOpt.orElse(existing.room),
              module = modOpt.orElse(existing.module),
              firmware = fwOpt.orElse(existing.firmware),
              device = devOpt.orElse(existing.device),
              group = grpOpt.orElse(existing.group),
              lastSeen = Some(now)
            )
            (updated.toLight, map.updated(address, updated))
          }
          _ <- hub.publish((updatedLight, Some(Source.Heartbeat)))
          _ <- network.publish(Outgoing.Send(Set(senderIp), GetPilotRequest))
          _ <- registerWith(senderIp)
        yield ()
    )

  /**
   * Handles a getSystemConfig response.
   *
   * @param senderIp The IP address of the sender.
   * @param payload  The getSystemConfig payload.
   */
  private def handleSystemConfig(senderIp: InetAddress, payload: GetSystemConfigResponsePayload): UIO[Unit] =
    Address.make(payload.mac).foldZIO(
      err => ZIO.logWarning(s"Invalid MAC in getSystemConfig from $senderIp: ${err.getMessage}"),
      address =>
        for
          homeOpt <- payload.homeId.fold(ZIO.none)(h => Home.make(h).option)
          roomOpt <- payload.roomId.fold(ZIO.none)(r => Room.make(r).option)
          modOpt <- payload.moduleName.fold(ZIO.none)(m => Module.make(m).option)
          fwOpt <- payload.fwVersion.fold(ZIO.none)(f => Firmware.make(f).option)
          devOpt <- payload.typeId.fold(ZIO.none)(d => Device.make(d).option)
          grpOpt <- payload.groupId.fold(ZIO.none)(g => Group.make(g).option)
          now <- Clock.instant
          (updatedLight, needsPilot) <- recordsRef.modify { map =>
            val existing = map.getOrElse(address, LightingLive.Record(address))
            val updated = existing.copy(
              inetAddress = Some(senderIp),
              signal = existing.signal.orElse(Some(Signal.Medium)),
              home = homeOpt.orElse(existing.home),
              room = roomOpt.orElse(existing.room),
              module = modOpt.orElse(existing.module),
              firmware = fwOpt.orElse(existing.firmware),
              device = devOpt.orElse(existing.device),
              group = grpOpt.orElse(existing.group),
              lastSeen = Some(now)
            )
            ((updated.toLight, existing.status.isEmpty), map.updated(address, updated))
          }
          _ <- hub.publish((updatedLight, None))
          _ <- if needsPilot then network.publish(Outgoing.Send(Set(senderIp), GetPilotRequest)) else ZIO.unit
          _ <- registerWith(senderIp)
        yield ()
    )

  /**
   * Handles a getPilot response.
   *
   * @param senderIp The IP address of the sender.
   * @param payload  The getPilot payload.
   */
  private def handleGetPilot(senderIp: InetAddress, payload: GetPilotResponsePayload): UIO[Unit] =
    Address.make(payload.mac).foldZIO(
      err => ZIO.logWarning(s"Invalid MAC in getPilot from $senderIp: ${err.getMessage}"),
      address =>
        for
          status <- LightingLive.extractStatus(
            payload.state,
            payload.dimming,
            payload.sceneId,
            payload.speed,
            payload.temp,
            payload.r,
            payload.g,
            payload.b,
            payload.c,
            payload.w
          )
          signal <- LightingLive.parseSignal(payload.rssi)
          now <- Clock.instant
          (updatedLight, needsConfig) <- recordsRef.modify { map =>
            val existing = map.getOrElse(address, LightingLive.Record(address))
            val updated = existing.copy(
              inetAddress = Some(senderIp),
              status = Some(status),
              signal = Some(signal),
              lastSeen = Some(now)
            )
            ((updated.toLight, existing.module.isEmpty), map.updated(address, updated))
          }
          _ <- hub.publish((updatedLight, None))
          _ <- if needsConfig then network.publish(Outgoing.Send(Set(senderIp), GetSystemConfigRequest)) else ZIO.unit
          _ <- registerWith(senderIp)
        yield ()
    )

  /**
   * Handles a syncPilot response.
   *
   * @param senderIp The IP address of the sender.
   * @param payload  The syncPilot payload.
   */
  private def handleSyncPilot(senderIp: InetAddress, payload: SyncPilotResponsePayload): UIO[Unit] =
    Address.make(payload.mac).foldZIO(
      err => ZIO.logWarning(s"Invalid MAC in syncPilot from $senderIp: ${err.getMessage}"),
      address =>
        for
          status <- LightingLive.extractStatus(
            payload.state,
            payload.dimming,
            payload.sceneId,
            payload.speed,
            payload.temp,
            payload.r,
            payload.g,
            payload.b,
            payload.c,
            payload.w
          )
          signal <- LightingLive.parseSignal(payload.rssi)
          source <- payload.src.fold(ZIO.none)(s => Source.make(s).option)
          now <- Clock.instant
          (updatedLight, needsConfig) <- recordsRef.modify { map =>
            val existing = map.getOrElse(address, LightingLive.Record(address))
            val updated = existing.copy(
              inetAddress = Some(senderIp),
              status = Some(status),
              signal = Some(signal),
              lastSeen = Some(now)
            )
            ((updated.toLight, existing.module.isEmpty), map.updated(address, updated))
          }
          _ <- hub.publish((updatedLight, source))
          _ <- if needsConfig then network.publish(Outgoing.Send(Set(senderIp), GetSystemConfigRequest)) else ZIO.unit
        yield ()
    )

  /** Converts a status into a SetPilotRequest. */
  private def statusToSetPilotRequest(status: Status): SetPilotRequest = status match
    case Status.Disabled =>
      SetPilotRequest(state = Some(false))
    case Status.White(temp, brightness) =>
      SetPilotRequest(state = Some(true), temp = Some(temp: Int), dimming = Some(brightness: Int))
    case Status.Defined.Static(scene, brightness) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), dimming = Some(brightness: Int))
    case Status.Defined.Dynamic(scene, speed, brightness) =>
      SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed: Int), dimming = Some(brightness: Int))
    case Status.Custom(color, brightness) =>
      SetPilotRequest(
        state = Some(true),
        dimming = Some(brightness: Int),
        r = Some(color.red),
        g = Some(color.green),
        b = Some(color.blue),
        c = Some(color.cool),
        w = Some(color.warm)
      )

/**
 * Factory for live lighting services.
 */
object LightingLive:

  /** The layer that provides the live lighting service. */
  val layer: RLayer[Configuration, Lighting] =
    Network.layer >>> ZLayer.scoped {
      for
        config <- ZIO.service[Configuration]
        net <- ZIO.service[Network]
        service <- make(config, net)
      yield service
  }

  /**
   * Creates a live lighting service.
   *
   * @param configuration The backend configuration.
   * @param network       The network service.
   * @return A live lighting service.
   */
  def make(configuration: Configuration, network: Network): URIO[Scope, LightingLive] = for
    hub <- Hub.unbounded[(Light, Option[Source])]
    recordsRef <- Ref.make(Map.empty[Address, Record])
    listenersRef <- Ref.make(Set.empty[InetAddress => UIO[Unit]])
    service = LightingLive(configuration, network, hub, recordsRef, listenersRef)
    _ <- network.subscribe.flatMap { stream =>
      stream.foreach(service.handleIncoming).forkScoped
    }
    _ <- service.discover
    _ <- service.discover
      .delay(Duration.fromScala(configuration.discoveryInterval))
      .repeat(Schedule.spaced(Duration.fromScala(configuration.discoveryInterval)))
      .forkScoped
  yield service

  /**
   * Internal record representing the accumulated state of a light.
   *
   * @param address     The MAC address of the light.
   * @param inetAddress The IP address of the light if known.
   * @param status      The status of the light if known.
   * @param signal      The signal strength of the light if known.
   * @param module      The module name of the light if known.
   * @param firmware    The firmware version of the light if known.
   * @param device      The device type ID of the light if known.
   * @param lastSeen    The timestamp when the light was last seen if known.
   * @param home        The home the light belongs to if known.
   * @param room        The room the light belongs to if known.
   * @param group       The group the light belongs to if known.
   */
  final case class Record(
    address: Address,
    inetAddress: Option[InetAddress] = None,
    status: Option[Status] = None,
    signal: Option[Signal] = None,
    module: Option[Module] = None,
    firmware: Option[Firmware] = None,
    device: Option[Device] = None,
    lastSeen: Option[Instant] = None,
    home: Option[Home] = None,
    room: Option[Room] = None,
    group: Option[Group] = None
  ):

    /** Converts this record into a Light model object. */
    def toLight: Light =
      val info = for
        ip <- inetAddress
        sig <- signal
        mod <- module
        fw <- firmware
        dev <- device
        seen <- lastSeen
        h <- home
        r <- room
      yield Info(ip, sig, mod, fw, dev, seen, h, r, group)
      Light(address, status, info)

  /**
   * Parses an RSSI value into a Signal model.
   *
   * @param rssi The raw RSSI value.
   * @return The parsed Signal value.
   */
  def parseSignal(rssi: Int): UIO[Signal] =
    Signal.make(rssi).orElseSucceed {
      if rssi < Signal.Weakest then Signal.Weakest
      else if rssi > Signal.Strongest then Signal.Strongest
      else Signal.Medium
    }

  /**
   * Extracts a Status from raw pilot parameters.
   *
   * @param state   Whether the light is on.
   * @param dimming The brightness level if present.
   * @param sceneId The scene ID if present.
   * @param speed   The dynamic scene speed if present.
   * @param temp    The color temperature if present.
   * @param r       The red component if present.
   * @param g       The green component if present.
   * @param b       The blue component if present.
   * @param c       The cool white component if present.
   * @param w       The warm white component if present.
   * @return The extracted Status.
   */
  def extractStatus(
    state: Boolean,
    dimming: Option[Int],
    sceneId: Option[Int],
    speed: Option[Int],
    temp: Option[Int],
    r: Option[Int],
    g: Option[Int],
    b: Option[Int],
    c: Option[Int],
    w: Option[Int]
  ): UIO[Status] =
    if !state then ZIO.succeed(Status.Disabled)
    else
      for
        brightness <- dimming.fold(ZIO.succeed(Brightness.Brightest)) { d =>
          if d < Brightness.Dimmest then ZIO.succeed(Brightness.Dimmest)
          else if d > Brightness.Brightest then ZIO.succeed(Brightness.Brightest)
          else Brightness.make(d).orElseSucceed(Brightness.Brightest)
        }
        status <- sceneId.flatMap(Scene.valueBy) match
          case Some(dynamic: Scene.Dynamic) =>
            val getSpeed = speed.fold(ZIO.succeed(Speed.Medium)) { s =>
              if s < Speed.Slowest then ZIO.succeed(Speed.Slowest)
              else if s > Speed.Fastest then ZIO.succeed(Speed.Fastest)
              else Speed.make(s).orElseSucceed(Speed.Medium)
            }
            getSpeed.map(spd => Status.Defined.Dynamic(dynamic, spd, brightness))
          case Some(static: Scene.Static) =>
            ZIO.succeed(Status.Defined.Static(static, brightness))
          case None =>
            temp match
              case Some(temperatureValue) =>
                Temperature.make(temperatureValue)
                  .orElseSucceed {
                    if temperatureValue < Temperature.Warmest then Temperature.Warmest
                    else if temperatureValue > Temperature.Coolest then Temperature.Coolest
                    else Temperature.Medium
                  }
                  .map(t => Status.White(t, brightness))
              case None =>
                if r.isDefined || g.isDefined || b.isDefined || c.isDefined || w.isDefined then
                  val red = math.max(0, math.min(255, r.getOrElse(0)))
                  val green = math.max(0, math.min(255, g.getOrElse(0)))
                  val blue = math.max(0, math.min(255, b.getOrElse(0)))
                  val cool = math.max(0, math.min(255, c.getOrElse(0)))
                  val warm = math.max(0, math.min(255, w.getOrElse(0)))
                  Color.make(red, green, blue, cool, warm).orElseSucceed(Color.White).map(Status.Custom(_, brightness))
                else
                  ZIO.succeed(Status.White(Temperature.Medium, brightness))
      yield status
