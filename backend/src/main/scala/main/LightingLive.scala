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
package main

import java.net.{Inet4Address, InetAddress, NetworkInterface}
import java.util.concurrent.TimeUnit

import scala.annotation.targetName
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*

import zio.stream.{UStream, ZStream}
import zio.{Cause, Clock, Duration, Hub, IO, Ref, Schedule, Scope, UIO, URIO, URLayer, ZIO, ZLayer}
import model.*
import network.{Incoming, Network, Outgoing}
import protocol.*
import service.Lighting

/**
 * The live implementation of the lighting service.
 *
 * @param retryBackoffs     The durations to wait between retrying outgoing messages.
 * @param clock             The clock service.
 * @param network           The network service.
 * @param events            The hub of light events.
 * @param devices           The reference to known lights.
 * @param responseListeners The reference to active response listeners for retries.
 */
final class LightingLive private(
  retryBackoffs: List[Duration],
  clock: Clock,
  network: Network,
  events: Hub[(Light, Option[Source])],
  devices: Ref[Map[MacAddress, DeviceState]],
  responseListeners: Ref[Set[InetAddress => UIO[Unit]]]
) extends Lighting:

  /** The number of milliseconds since the epoch. */
  private val currentTime: UIO[Long] = clock.currentTime(TimeUnit.MILLISECONDS)

  //
  // Service implementation methods.
  //

  /* Return the lights managed by this service. */
  override def lights: UIO[Set[Light]] =
    devices.get.map(_.values.map(_.toLight).toSet)

  /* Return the light with the specified MAC address. */
  override def get(macAddress: MacAddress): UIO[Light] =
    devices.get.map(_.get(macAddress).map(_.toLight).getOrElse(Light(macAddress, None, None)))

  /* Return the lights with the specified MAC addresses. */
  override def getAll(macAddresses: Iterable[MacAddress]): UIO[Set[Light]] =
    devices.get.map { map =>
      macAddresses.map(macAddress => map.get(macAddress).map(_.toLight).getOrElse(Light(macAddress, None, None))).toSet
    }

  /* Update the enabled state of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, enabled: Boolean): UIO[Boolean] =
    updateAll(List(macAddress), enabled).map(_ > 0)

  /* Update the enabled state of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], enabled: Boolean): UIO[Int] =
    executeUpdate(
      macAddresses,
      SetPilotRequest(state = Some(enabled)),
      current =>
        if enabled then current.map {
          case Status.Disabled => Status.White(Temperature.Medium, Brightness.Brightest)
          case other => other
        } else Some(Status.Disabled)
    )

  /* Update the brightness of the light at the specified MAC address. */
  @targetName("updateBrightness")
  override def update(macAddress: MacAddress, brightness: Brightness): UIO[Boolean] =
    updateAll(List(macAddress), brightness).map(_ > 0)

  /* Update the brightness of the lights at the specified MAC addresses. */
  @targetName("updateAllBrightness")
  override def updateAll(macAddresses: Iterable[MacAddress], brightness: Brightness): UIO[Int] =
    executeUpdate(
      macAddresses,
      SetPilotRequest(dimming = Some(brightness)),
      {
        case Some(Status.White(t, _)) => Some(Status.White(t, brightness))
        case Some(Status.Defined.Static(s, _)) => Some(Status.Defined.Static(s, brightness))
        case Some(Status.Defined.Dynamic(s, sp, _)) => Some(Status.Defined.Dynamic(s, sp, brightness))
        case Some(Status.Custom(c, _)) => Some(Status.Custom(c, brightness))
        case Some(Status.Disabled) => Some(Status.White(Temperature.Medium, brightness))
        case None => Some(Status.White(Temperature.Medium, brightness))
      }
    )

  /* Update the color temperature of the light at the specified MAC address. */
  @targetName("updateTemperature")
  override def update(macAddress: MacAddress, temperature: Temperature): UIO[Boolean] =
    updateAll(List(macAddress), temperature).map(_ > 0)

  /* Update the color temperature of the lights at the specified MAC addresses. */
  @targetName("updateAllTemperature")
  override def updateAll(macAddresses: Iterable[MacAddress], temperature: Temperature): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), temp = Some(temperature: Int))
    executeUpdate(
      macAddresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.White(temperature, b))
    )

  /* Update the color of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, color: Color): UIO[Boolean] =
    updateAll(List(macAddress), color).map(_ > 0)

  /* Update the color of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], color: Color): UIO[Int] =
    val request = SetPilotRequest(
      state = Some(true),
      r = Some(color.red),
      g = Some(color.green),
      b = Some(color.blue),
      c = Some(color.cool),
      w = Some(color.warm)
    )
    executeUpdate(
      macAddresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.Custom(color, b))
    )

  /* Update the scene of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, scene: Scene): UIO[Boolean] =
    updateAll(List(macAddress), scene).map(_ > 0)

  /* Update the scene of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), sceneId = Some(scene.id))
    executeUpdate(
      macAddresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        scene match
          case dynamic: Scene.Dynamic =>
            val sp = current.collect { case d: Status.Defined.Dynamic => d.speed }.getOrElse(Speed.Medium)
            Some(Status.Defined.Dynamic(dynamic, sp, b))
          case static: Scene.Static =>
            Some(Status.Defined.Static(static, b))
    )

  /* Update the dynamic scene and speed of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, scene: Scene.Dynamic, speed: Speed): UIO[Boolean] =
    updateAll(List(macAddress), scene, speed).map(_ > 0)

  /* Update the dynamic scene and speed of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene.Dynamic, speed: Speed): UIO[Int] =
    val request = SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed: Int))
    executeUpdate(
      macAddresses,
      request,
      current =>
        val b = current.collect { case e: Status.Enabled => e.brightness }.getOrElse(Brightness.Brightest)
        Some(Status.Defined.Dynamic(scene, speed, b))
    )

  /* Update the dynamic scene speed of the light at the specified MAC address. */
  @targetName("updateSpeed")
  override def update(macAddress: MacAddress, speed: Speed): UIO[Boolean] =
    updateAll(List(macAddress), speed).map(_ > 0)

  /* Update the dynamic scene speed of the lights at the specified MAC addresses. */
  @targetName("updateAllSpeed")
  override def updateAll(macAddresses: Iterable[MacAddress], speed: Speed): UIO[Int] =
    executeUpdate(
      macAddresses,
      SetPilotRequest(speed = Some(speed)),
      {
        case Some(Status.Defined.Dynamic(s, _, b)) => Some(Status.Defined.Dynamic(s, speed, b))
        case other => other
      }
    )

  /* Update the status of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, status: Status): UIO[Boolean] =
    updateAll(List(macAddress), status).map(_ > 0)

  /* Update the statuses of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], status: Status): UIO[Int] =
    executeUpdate(
      macAddresses,
      status match
        case Status.Disabled =>
          SetPilotRequest(state = Some(false))
        case Status.White(temp, brightness) =>
          SetPilotRequest(state = Some(true), temp = Some(temp), dimming = Some(brightness))
        case Status.Defined.Static(scene, brightness) =>
          SetPilotRequest(state = Some(true), sceneId = Some(scene.id), dimming = Some(brightness))
        case Status.Defined.Dynamic(scene, speed, brightness) =>
          SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed), dimming = Some(brightness))
        case Status.Custom(color, brightness) =>
          SetPilotRequest(
            state = Some(true),
            dimming = Some(brightness),
            r = Some(color.red),
            g = Some(color.green),
            b = Some(color.blue),
            c = Some(color.cool),
            w = Some(color.warm)
          ),
      _ => Some(status)
    )

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[(Light, Option[Source])]] =
    ZStream.fromHubScoped(events)

  //
  // Handling of outgoing messages.
  //

  /** Broadcasts discovery requests on the network. */
  def discover: UIO[Unit] = for
    _ <- network.publish(Outgoing.Broadcast(GetSystemConfigRequest))
    _ <- network.publish(Outgoing.Broadcast(GetPilotRequest))
    credentials <- network.localhost
    _ <- credentials match
      case Some((mac, ip)) => network.publish(Outgoing.Broadcast(RegistrationRequest(mac, ip)))
      case None => ZIO.unit
  yield ()

  /**
   * Executes a partial or full status update on lights matching the specified addresses.
   *
   * @param addresses    The addresses of the lights to update.
   * @param request      The pilot request to send.
   * @param updateStatus The function to update the local cached status.
   * @return The number of lights found and updated.
   */
  private def executeUpdate(
    addresses: Iterable[MacAddress],
    request: SetPilotRequest,
    updateStatus: Option[Status] => Option[Status]
  ): UIO[Int] = for
    targetAddresses <- ZIO.succeed(addresses.toSet)
    currentRecords <- devices.get
    matching = targetAddresses.flatMap { addr =>
      currentRecords.get(addr).flatMap(rec => rec.inetAddress.map(ip => (addr, rec, ip)))
    }
    _ <- if matching.isEmpty then ZIO.unit else
      val ips = matching.map(_._3)
      for
        now <- Clock.instant
        updatedLights <- devices.modify { map =>
          val updated = matching.foldLeft(map) { case (acc, (addr, rec, _)) =>
            val newStatus = updateStatus(rec.status)
            val newRec = rec.copy(status = newStatus)// FIXME, lastSeen = Some(now))
            acc.updated(addr, newRec)
          }
          val lights = matching.flatMap { case (addr, _, _) => updated.get(addr).map(_.toLight) }
          (lights, updated)
        }
        _ <- sendCommand(ips, request)
        _ <- ZIO.foreachDiscard(updatedLights) { light =>
          events.publish((light, Some(Source.Udp)))
        }
      yield ()
  yield matching.size

  /**
   * Sends an outgoing command with retries that track responding devices.
   *
   * @param targets The target IP addresses.
   * @param request The request to send.
   */
  private def sendCommand(targets: Set[InetAddress], request: Request): UIO[Unit] =
    if targets.isEmpty then ZIO.unit
    else if retryBackoffs.isEmpty then
      network.publish(Outgoing.Send(targets, request))
    else
      for
        pendingRef <- Ref.make(targets)
        listener = (ip: InetAddress) => pendingRef.update(_ - ip)
        _ <- responseListeners.update(_ + listener)
        _ <- network.publish(Outgoing.Send(targets, request))
        _ <- retryLoop(pendingRef, request, retryBackoffs)
          .ensuring(responseListeners.update(_ - listener))
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
    backoffs: List[Duration]
  ): UIO[Unit] = backoffs match
    case Nil => ZIO.unit
    case head :: tail =>
      ZIO.sleep(head) *> pendingRef.get.flatMap { remaining =>
        if remaining.isEmpty then ZIO.unit
        else network.publish(Outgoing.Send(remaining, request)) *> retryLoop(pendingRef, request, tail)
      }

  /**
   * Registers this controller with the specified device IP.
   *
   * @param target The device IP address to register with.
   */
  private def registerWith(target: InetAddress): UIO[Unit] =
    network.localhost.flatMap {
      case Some((mac, ip)) => network.publish(Outgoing.Send(Set(target), RegistrationRequest(mac, ip)))
      case None => ZIO.unit
    }

  /**
   * Notifies active retry listeners that a device at the specified IP address has responded.
   *
   * @param senderIp The IP address of the responding device.
   */
  private def notifyResponse(senderIp: InetAddress): UIO[Unit] =
    responseListeners.get.flatMap { listeners =>
      if listeners.isEmpty then ZIO.unit
      else ZIO.foreachDiscard(listeners)(_(senderIp))
    }

  //
  // Handling of incoming messages.
  //

  /**
   * Called when the service receives a message.
   *
   * @param message The incoming message.
   */
  def onMessage(message: Incoming): UIO[Unit] =
    notifyResponse(message.address) *> { // FIXME
      message.response match
        case FirstBeatResponse(payload) =>
          payload.fold(onErrorResponse(FirstBeatMethod, _), onFirstBeat(message.address, _))
        case GetSystemConfigResponse(payload) =>
          payload.fold(onErrorResponse(GetSystemConfigMethod, _), onGetSystemConfig(message.address, _))
        case GetPilotResponse(payload) =>
          payload.fold(onErrorResponse(GetPilotMethod, _), onGetPilot(message.address, _))
        case SyncPilotResponse(payload) =>
          payload.fold(onErrorResponse(SyncPilotMethod, _), onSyncPilot(message.address, _))
        case RegistrationResponse(payload) =>
          payload.fold(onErrorResponse(RegistrationMethod, _), onRegistration(message.address, _))
        case SetPilotResponse(payload) =>
          payload.fold(onErrorResponse(SetPilotMethod, _), onSetPilot(message.address, _))
    }

  /**
   * Called when the service receives a first beat response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The first beat response payload.
   */
  private def onFirstBeat(inetAddress: InetAddress, payload: FirstBeatResponsePayload): UIO[Unit] = for
    now <- currentTime
    changes <- DeviceChanges.Settings.make(
      payload.mac,
      now,
      payload.homeId,
      payload.roomId,
      payload.moduleName,
      payload.fwVersion,
      payload.groupId,
      payload.typeId
    ).asSome.catchAllCause(ZIO.logWarningCause("Unable to interpret first beat changes.", _).map(_ => None))
    _ <- changes.fold(ZIO.unit) { changes =>
      for
        newDevice <- applyChanges(inetAddress, changes, Some(Source.Heartbeat))
        _ <- network.publish(Outgoing.Send(Set(inetAddress), GetPilotRequest)) // FIXME Network.publish retries?
        _ <- registerWith(inetAddress) // FIXME What is this doing?
      yield ()
    }
  yield ()

  /**
   * Handles a getSystemConfig response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The getSystemConfig payload.
   */
  private def onGetSystemConfig(inetAddress: InetAddress, payload: GetSystemConfigResponsePayload): UIO[Unit] = for
    now <- currentTime
    changes <- DeviceChanges.Settings.make(
      payload.mac,
      now,
      payload.homeId,
      payload.roomId,
      payload.moduleName,
      payload.fwVersion,
      payload.groupId,
      payload.typeId
    ).asSome.catchAllCause(ZIO.logWarningCause("Unable to interpret get system config changes.", _).map(_ => None))
    _ <- changes.fold(ZIO.unit) { changes =>
      for
        newDevice <- applyChanges(inetAddress, changes, None)
        _ <- if newDevice.status.isEmpty then network.publish(Outgoing.Send(Set(inetAddress), GetPilotRequest)) else ZIO.unit
        _ <- registerWith(inetAddress)
      yield ()
    }
  yield ()

  /**
   * Handles a getPilot response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The getPilot payload.
   */
  private def onGetPilot(inetAddress: InetAddress, payload: GetPilotResponsePayload): UIO[Unit] = for
    now <- currentTime
    changes <- DeviceChanges.Pilot.make(
      payload.mac,
      now,
      payload.rssi,
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
    ).asSome.catchAllCause(ZIO.logWarningCause("Unable to interpret get pilot changes.", _).map(_ => None))
    _ <- changes.fold(ZIO.unit) { changes =>
      for
        newDevice <- applyChanges(inetAddress, changes, None)
        _ <- if newDevice.module.isEmpty then network.publish(Outgoing.Send(Set(inetAddress), GetSystemConfigRequest)) else ZIO.unit
        _ <- registerWith(inetAddress)
      yield ()
    }
  yield ()

  /**
   * Handles a syncPilot response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The syncPilot payload.
   */
  private def onSyncPilot(inetAddress: InetAddress, payload: SyncPilotResponsePayload): UIO[Unit] = for
    now <- currentTime
    changes <- DeviceChanges.Pilot.make(
      payload.mac,
      now,
      payload.rssi,
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
    ).asSome.catchAllCause(ZIO.logWarningCause("Unable to interpret sync pilot changes.", _).map(_ => None))
    source <- payload.src.fold(ZIO.none)(Source.make(_).asSome.catchAllCause(_ => ZIO.none))
    _ <- changes.fold(ZIO.unit) { changes =>
      for
        newDevice <- applyChanges(inetAddress, changes, source)
        _ <- if newDevice.module.isEmpty then network.publish(Outgoing.Send(Set(inetAddress), GetSystemConfigRequest)) else ZIO.unit
      yield ()
    }
  yield ()

  private def onRegistration(inetAddress: InetAddress, payload: RegistrationResponsePayload): UIO[Unit] =
    ???

  private def onSetPilot(inetAddress: InetAddress, payload: SetPilotResponsePayload): UIO[Unit] =
    ???

  /**
   * Applies a set of changes to a device state in response to a signal from that device.
   *
   * @param inetAddress The internet address of the device in question.
   * @param changes     The changes to apply to the device state.
   * @param source      The optional source of the event that generated the changes.
   * @return The new state of the device in question.
   */
  private def applyChanges(
    inetAddress: InetAddress,
    changes: DeviceChanges,
    source: Option[Source]
  ): UIO[DeviceState] = for
    (oldDevice, newDevice) <- devices.modify { devices =>
      val oldDevice = devices.getOrElse(changes.macAddress, DeviceState(changes.macAddress))
      val newDevice = oldDevice.merge(inetAddress, changes)
      (oldDevice -> newDevice, devices.updated(changes.macAddress, newDevice))
    }
    _ <-
      if newDevice.timeless != oldDevice.timeless then events.publish(newDevice.toLight -> source).ignore
      else ZIO.unit
  yield newDevice

  /**
   * Called when a device has sent an error response.
   *
   * @param method The method that the error pertains to.
   * @param error The payload of the error response.
   */
  private def onErrorResponse(method: String, error: ErrorResponsePayload): UIO[Unit] =
    (error.code, error.message) match
      case (Some(code), Some(message)) => ZIO.logWarning(s"Received $method response error $code: $message")
      case (Some(code), None) => ZIO.logWarning(s"Received $method response error $code.")
      case (None, Some(message)) => ZIO.logWarning(s"Received $method response error: $message")
      case (None, None) => ZIO.logWarning(s"Received $method response error.")

/**
 * Factory for live lighting services.
 */
object LightingLive:

  /** The layer that provides the live lighting service. */
  val layer: URLayer[Configuration & Clock & Network, Lighting] = ZLayer.scoped {
    for
      configuration <- ZIO.service[Configuration]
      clock <- ZIO.service[Clock]
      network <- ZIO.service[Network]
      service <- make(configuration.discoveryInterval, configuration.retryBackoffs, clock, network)
    yield service
  }

  /**
   * Creates a live lighting service.
   *
   * @param discoveryInterval The frequency of broadcast discovery attempts.
   * @param retryBackoffs     The durations to wait between retrying outgoing messages.
   * @param clock             The clock service.
   * @param network           The network service.
   * @return A live lighting service.
   */
  def make(
    discoveryInterval: FiniteDuration,
    retryBackoffs: List[FiniteDuration],
    clock: Clock,
    network: Network
  ): URIO[Scope, Lighting] = for
    hub <- Hub.unbounded[(Light, Option[Source])]
    devices <- Ref.make(Map.empty[MacAddress, DeviceState])
    listeners <- Ref.make(Set.empty[InetAddress => UIO[Unit]])
    service = LightingLive(retryBackoffs.map(Duration.fromScala), clock, network, hub, devices, listeners)
    _ <- network.subscribe.flatMap(_.foreach(service.onMessage).forkScoped)
    discoveryIntervalZio = Duration.fromScala(discoveryInterval)
    _ <- service.discover
    _ <- service.discover.repeat(Schedule.spaced(discoveryIntervalZio)).delay(discoveryIntervalZio).forkScoped
  yield service
