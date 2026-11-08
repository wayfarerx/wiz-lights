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

import java.net.InetAddress
import java.util.concurrent.TimeUnit

import scala.annotation.targetName
import scala.concurrent.duration.*

import zio.{Clock, Duration, Hub, Ref, Schedule, Scope, UIO, URIO, URLayer, ZIO, ZLayer}
import zio.concurrent.ConcurrentMap
import zio.stream.{UStream, ZStream}

import model.*
import network.*
import protocol.*
import service.*

/**
 * The live implementation of the lighting service.
 *
 * @param registrationTtl The duration after which a bulb registration is refreshed.
 * @param retryBackoffs   The durations to wait between retrying outgoing messages.
 * @param purgeTimeout    The timeout for devices that do not respond.
 * @param clock           The clock service.
 * @param network         The network service.
 * @param events          The hub of light events.
 * @param devices         The reference to known lights.
 * @param invocations     The reference to active response listeners for retries.
 */
final class LightingLive private(
  registrationTtl: Duration,
  retryBackoffs: List[Duration],
  purgeTimeout: Duration,
  clock: Clock,
  network: Network,
  events: Hub[(Light, Option[Source])],
  devices: Ref.Synchronized[Devices],
  invocations: ConcurrentMap[Int, Invocation]
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
  override def getAll(macAddresses: Iterable[MacAddress]): UIO[Map[MacAddress, Light]] =
    devices.get.map { map =>
      macAddresses.foldLeft(Map.empty[MacAddress, Light]) { (result, macAddress) =>
        map.get(macAddress).fold(result)(state => result + (macAddress -> state.toLight))
      }
    }

  /* Update the enabled state of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, enabled: Boolean): UIO[Unit] =
    updateAll(List(macAddress), enabled)

  /* Update the enabled state of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], enabled: Boolean): UIO[Unit] =
    updateAll(macAddresses, Update.Enabled(enabled))

  /* Update the brightness of the light at the specified MAC address. */
  @targetName("updateBrightness")
  override def update(macAddress: MacAddress, brightness: Brightness): UIO[Unit] =
    updateAll(List(macAddress), brightness)

  /* Update the brightness of the lights at the specified MAC addresses. */
  @targetName("updateAllBrightness")
  override def updateAll(macAddresses: Iterable[MacAddress], brightness: Brightness): UIO[Unit] =
    updateAll(macAddresses, Update.Brightness(brightness))

  /* Update the color temperature of the light at the specified MAC address. */
  @targetName("updateTemperature")
  override def update(macAddress: MacAddress, temperature: Temperature): UIO[Unit] =
    updateAll(List(macAddress), temperature)

  /* Update the color temperature of the lights at the specified MAC addresses. */
  @targetName("updateAllTemperature")
  override def updateAll(macAddresses: Iterable[MacAddress], temperature: Temperature): UIO[Unit] =
    updateAll(macAddresses, Update.Temperature(temperature))

  /* Update the color of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, color: Color): UIO[Unit] =
    updateAll(List(macAddress), color)

  /* Update the color of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], color: Color): UIO[Unit] =
    updateAll(macAddresses, Update.Color(color))

  /* Update the scene of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, scene: Scene): UIO[Unit] =
    updateAll(List(macAddress), scene)

  /* Update the scene of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene): UIO[Unit] =
    updateAll(macAddresses, Update.Scene(scene))

  /* Update the dynamic scene and speed of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, scene: Scene.Dynamic, speed: Speed): UIO[Unit] =
    updateAll(List(macAddress), scene, speed)

  /* Update the dynamic scene and speed of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], scene: Scene.Dynamic, speed: Speed): UIO[Unit] =
    updateAll(macAddresses, Update.Dynamic(scene, speed))

  /* Update the dynamic scene speed of the light at the specified MAC address. */
  @targetName("updateSpeed")
  override def update(macAddress: MacAddress, speed: Speed): UIO[Unit] =
    updateAll(List(macAddress), speed)

  /* Update the dynamic scene speed of the lights at the specified MAC addresses. */
  @targetName("updateAllSpeed")
  override def updateAll(macAddresses: Iterable[MacAddress], speed: Speed): UIO[Unit] =
    updateAll(macAddresses, Update.Speed(speed))

  /* Update the status of the light at the specified MAC address. */
  override def update(macAddress: MacAddress, status: Status): UIO[Unit] =
    updateAll(List(macAddress), status)

  /* Update the statuses of the lights at the specified MAC addresses. */
  override def updateAll(macAddresses: Iterable[MacAddress], status: Status): UIO[Unit] =
    updateAll(macAddresses, Update.Status(status))

  /* Update the light at the specified MAC address using the specified update. */
  override def update(macAddress: MacAddress, update: Update): UIO[Unit] =
    updateAll(List(macAddress), update)

  /* Update the lights at the specified MAC addresses using the specified update. */
  override def updateAll(macAddresses: Iterable[MacAddress], update: Update): UIO[Unit] = update match
    case Update.Enabled(enabled) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(state = Some(enabled)))
    case Update.Brightness(brightness) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(dimming = Some(brightness)))
    case Update.Temperature(temperature) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(state = Some(true), temp = Some(temperature)))
    case Update.Color(color) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(
        state = Some(true),
        r = Some(color.red),
        g = Some(color.green),
        b = Some(color.blue),
        c = Some(color.cool),
        w = Some(color.warm)
      ))
    case Update.Scene(scene) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(state = Some(true), sceneId = Some(scene.id)))
    case Update.Dynamic(scene, speed) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(state = Some(true), sceneId = Some(scene.id), speed = Some(speed)))
    case Update.Speed(speed) =>
      invoke(macAddresses, Some(update))(SetPilotRequest(speed = Some(speed)))
    case Update.Status(status) =>
      invoke(macAddresses, Some(update))(
        status match
          case Status.Disabled =>
            SetPilotRequest(
              state = Some(false)
            )
          case Status.White(temp, brightness) =>
            SetPilotRequest(
              state = Some(true),
              temp = Some(temp),
              dimming = Some(brightness)
            )
          case Status.Defined.Static(scene, brightness) =>
            SetPilotRequest(
              state = Some(true),
              sceneId = Some(scene.id),
              dimming = Some(brightness)
            )
          case Status.Defined.Dynamic(scene, speed, brightness) =>
            SetPilotRequest(
              state = Some(true),
              sceneId = Some(scene.id),
              speed = Some(speed),
              dimming = Some(brightness)
            )
          case Status.Custom(color, brightness) =>
            SetPilotRequest(
              state = Some(true),
              dimming = Some(brightness),
              r = Some(color.red),
              g = Some(color.green),
              b = Some(color.blue),
              c = Some(color.cool),
              w = Some(color.warm)
            )
      )

  /* Subscribe to events from this service. */
  override def subscribe: URIO[Scope, UStream[(Light, Option[Source])]] =
    ZStream.fromHubScoped(events)

  //
  // Helper functions.
  //

  /**
   * Looks up the internet addresses of the devices with the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the devices to look up.
   * @return The internet addresses of the devices with the specified MAC addresses.
   */
  private def lookup(macAddresses: Iterable[MacAddress]): UIO[Map[MacAddress, InetAddress]] =
    devices.get.map(_.lookup(macAddresses))

  /**
   * Resolves an incoming response to a target MAC address using its IP address
   * or the correlation ID of its invocation.
   *
   * @param inetAddress The IP address the response was received from.
   * @param id          The optional correlation ID included in the response.
   * @return The target MAC address if resolved.
   */
  private def resolve(inetAddress: InetAddress, id: Option[Int]): UIO[Option[MacAddress]] =
    val byId = id.fold[UIO[Option[MacAddress]]](ZIO.none) { invocationId =>
      invocations.get(invocationId).flatMap {
        case Some(invocation) =>
          invocation.target
        case None =>
          ZIO.none
      }
    }
    byId.flatMap {
      case Some(macAddress) =>
        ZIO.some(macAddress)
      case None =>
        devices.get.map(_.get(inetAddress))
    }

  /**
   * Sets the last seen timestamp of a device to the current time.
   *
   * @param macAddress The MAC address of the device being touched.
   */
  private def touch(macAddress: MacAddress): UIO[Unit] = for
    now <- currentTime
    _ <- devices.modify(devs => () -> devs.touch(macAddress, now))
  yield ()

  /**
   * Completes an invocation from a MAC address.
   *
   * @param id The possible ID of the invocation to complete.
   * @param macAddress The MAC address that completed the invocation.
   * @return The update associated with the invocation, if any.
   */
  private def completeInvocation(id: Option[Int], macAddress: MacAddress): UIO[Option[Update]] = for
    invocation <- id.fold[UIO[Option[Invocation]]](ZIO.none)(invocations.get)
    result <- invocation.fold[UIO[Option[Update]]](ZIO.none) { invocation =>
      for
        result <- invocation.complete(macAddress)
        target <- invocation.target
        _ <- if target.isEmpty then invocations.remove(invocation.id) else ZIO.unit
      yield result
    }
  yield result

  /** Purges outdated data. */
  private def purge: UIO[Unit] = for
    now <- currentTime
    cutoff = now - purgeTimeout.toMillis
    removedDevices <- devices.modify(_.purge(cutoff))
    _ <- ZIO.foreachDiscard(removedDevices) { state =>
      events.publish(Light(state.macAddress, None, None) -> None).ignore
    }
    activeInvocations <- invocations.toList
    _ <- ZIO.foreachDiscard(activeInvocations) { case (id, invocation) =>
      if invocation.createdAt < cutoff then invocations.remove(id).ignore else ZIO.unit
    }
  yield ()

  //
  // Handling of outgoing messages.
  //

  /**
   * Sends a message to the specified devices.
   *
   * @param targets The targets of the message.
   * @param update  The optional update associated with the message.
   * @param request The request to send as the body of the message.
   */
  private def invoke(targets: Iterable[MacAddress], update: Option[Update] = None)(request: Request): UIO[Unit] = for
    now <- currentTime
    _ <- ZIO.foreachDiscard(targets) { target =>
      for
        invocation <- Invocation.make(target, now, update)
        _ <- invocations.put(invocation.id, invocation)
        _ <- retryLoop(invocation, request.withId(Some(invocation.id)), retryBackoffs).fork
      yield ()
    }
  yield ()

  /**
   * Retries sending a request to devices that have not yet responded.
   *
   * @param invocation The invocation being managed.
   * @param request    The request to send.
   * @param backoffs   The remaining backoff durations.
   */
  private def retryLoop(
    invocation: Invocation,
    request: Request,
    backoffs: List[Duration]
  ): UIO[Unit] =
    invocation.target.flatMap {
      case None =>
        invocations.remove(invocation.id).ignore
      case Some(target) =>
        lookup(target :: Nil).flatMap { targets =>
          if targets.isEmpty then
            ZIO.logWarning(s"Unable to resolve IP address for $target.")
          else
            network.publish(Outgoing.Send(targets.values.toSet, request)) *> {
              backoffs match
                case head :: tail =>
                  ZIO.sleep(head) *> retryLoop(invocation, request, tail)
                case Nil =>
                  ZIO.unit
            }
        }
    }

  /**
   * Registers this controller with any devices that are not currently registered.
   */
  private def register: UIO[Unit] =
    devices.get.flatMap(devs => registerWith(devs.keys))

  /**
   * Registers this controller with the specified devices if they are not currently registered
   * or if their registration has expired.
   *
   * @param targets The device MAC addresses to consider registering with.
   * @param request The optional pre-constructed registration request.
   */
  private def registerWith(
    targets: Iterable[MacAddress],
    request: Option[RegistrationRequest] = None
  ): UIO[Unit] = for
    now <- currentTime
    cutoff = now - (registrationTtl.toMillis * 0.8).toLong
    devs <- devices.get
    unregisteredTargets = targets.filter { mac =>
      devs.get(mac).flatMap(_.registration).fold(true)(_ <= cutoff)
    }
    _ <- if unregisteredTargets.isEmpty then ZIO.unit else for
      registration <- request.fold {
        network.localhost.flatMap {
          case Some((mac, ip)) => ZIO.some(RegistrationRequest(mac, ip))
          case None => ZIO.none
        }
      }(ZIO.some)
      _ <- registration.fold(
        ZIO.logWarning(s"Unable to register with ${unregisteredTargets.mkString(", ")}: local host address unavailable.")
      )(invoke(unregisteredTargets)(_))
    yield ()
  yield ()

  //
  // Handling of incoming messages.
  //

  /**
   * Called when the service receives a message.
   *
   * @param message The incoming message.
   */
  def onMessage(message: Incoming): UIO[Unit] = message.response match
    case FirstBeatResponse(payload, _) =>
      payload.fold(onErrorResponse(FirstBeatMethod, message.address, None, _), onFirstBeat(message.address, _))
    case GetSystemConfigResponse(payload, id) =>
      payload.fold(onErrorResponse(GetSystemConfigMethod, message.address, id, _), onGetSystemConfig(message.address, id, _))
    case GetPilotResponse(payload, id) =>
      payload.fold(onErrorResponse(GetPilotMethod, message.address, id, _), onGetPilot(message.address, id, _))
    case SyncPilotResponse(payload, _) =>
      payload.fold(onErrorResponse(SyncPilotMethod, message.address, None, _), onSyncPilot(message.address, _))
    case RegistrationResponse(payload, id) =>
      payload.fold(onErrorResponse(RegistrationMethod, message.address, id, _), onRegistration(message.address, id, _))
    case SetPilotResponse(payload, id) =>
      payload.fold(onErrorResponse(SetPilotMethod, message.address, id, _), onSetPilot(message.address, id, _))

  /**
   * Called when the service receives a first beat response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The first beat response payload.
   */
  private def onFirstBeat(
    inetAddress: InetAddress,
    payload: FirstBeatResponsePayload
  ): UIO[Unit] = for
    now <- currentTime
    changes <- Changes.Settings.make(
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
      val macAddresses = changes.macAddress :: Nil
      for
        newDevice <- applyChanges(inetAddress, changes, Some(Source.Heartbeat))
        _ <- invoke(macAddresses)(GetPilotRequest())
        _ <- if newDevice.module.isEmpty || newDevice.firmware.isEmpty then invoke(macAddresses)(GetSystemConfigRequest()) else ZIO.unit
        _ <- registerWith(macAddresses)
      yield ()
    }
  yield ()

  /**
   * Handles a getSystemConfig response.
   *
   * @param inetAddress The internet address of the device.
   * @param id          The correlation ID of the incoming response.
   * @param payload     The getSystemConfig payload.
   */
  private def onGetSystemConfig(
    inetAddress: InetAddress,
    id: Option[Int],
    payload: GetSystemConfigResponsePayload
  ): UIO[Unit] = for
    now <- currentTime
    changes <- Changes.Settings.make(
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
      val macAddresses = changes.macAddress :: Nil
      for
        _ <- completeInvocation(id, changes.macAddress)
        newDevice <- applyChanges(inetAddress, changes, None)
        _ <- if newDevice.status.isEmpty then invoke(macAddresses)(GetPilotRequest()) else ZIO.unit
        _ <- registerWith(macAddresses)
      yield ()
    }
  yield ()

  /**
   * Handles a getPilot response.
   *
   * @param inetAddress The internet address of the device.
   * @param id          The correlation ID of the incoming response.
   * @param payload     The getPilot payload.
   */
  private def onGetPilot(
    inetAddress: InetAddress,
    id: Option[Int],
    payload: GetPilotResponsePayload
  ): UIO[Unit] = for
    now <- currentTime
    changes <- Changes.Pilot.make(
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
      val macAddresses = changes.macAddress :: Nil
      for
        _ <- completeInvocation(id, changes.macAddress)
        newDevice <- applyChanges(inetAddress, changes, None)
        _ <- if newDevice.module.isEmpty || newDevice.firmware.isEmpty then invoke(macAddresses)(GetSystemConfigRequest()) else ZIO.unit
        _ <- registerWith(macAddresses)
      yield ()
    }
  yield ()

  /**
   * Handles a syncPilot response.
   *
   * @param inetAddress The internet address of the device.
   * @param payload     The syncPilot payload.
   */
  private def onSyncPilot(
    inetAddress: InetAddress,
    payload: SyncPilotResponsePayload
  ): UIO[Unit] = for
    now <- currentTime
    changes <- Changes.Pilot.make(
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
      payload.w,
      payload.src
    ).asSome.catchAllCause(ZIO.logWarningCause("Unable to interpret sync pilot changes.", _).map(_ => None))
    _ <- changes.fold(ZIO.unit) { changes =>
      val macAddresses = changes.macAddress :: Nil
      for
        newDevice <- applyChanges(inetAddress, changes, changes.source)
        _ <- if newDevice.module.isEmpty || newDevice.firmware.isEmpty then invoke(macAddresses)(GetSystemConfigRequest()) else ZIO.unit
        _ <- registerWith(macAddresses)
      yield ()
    }
  yield ()

  /**
   * Handles a registration response.
   *
   * @param inetAddress The internet address of the device.
   * @param id          The correlation ID of the incoming response.
   * @param payload     The registration payload.
   */
  private def onRegistration(
    inetAddress: InetAddress,
    id: Option[Int],
    payload: RegistrationResponsePayload
  ): UIO[Unit] = for
    macAddress <- resolve(inetAddress, id)
    now <- currentTime
    _ <- macAddress.fold(ZIO.unit) { macAddress =>
      completeInvocation(id, macAddress) *> {
        if payload.success then applyChanges(inetAddress, Changes.Registered(macAddress, now), None)
        else touch(macAddress) *> ZIO.logWarning(s"A registration request failed for $macAddress.")
      }
    }
  yield ()

  /**
   * Handles a setPilot response.
   *
   * @param inetAddress The internet address of the device.
   * @param id          The correlation ID of the incoming response.
   * @param payload     The set pilot payload.
   */
  private def onSetPilot(
    inetAddress: InetAddress,
    id: Option[Int],
    payload: SetPilotResponsePayload
  ): UIO[Unit] = for
    macAddress <- resolve(inetAddress, id)
    _ <- macAddress.fold(ZIO.unit) { macAddress =>
      for
        update <- completeInvocation(id, macAddress)
        _ <- update.fold(touch(macAddress)) { update =>
          if payload.success then for
            now <- currentTime
            _ <- applyChanges(inetAddress, Changes.Updates(macAddress, now, update), Some(Source.Udp))
          yield () else touch(macAddress) *> ZIO.logWarning(s"A setPilot request failed: $update.")
        }
      yield ()
    }
  yield ()

  /**
   * Applies a set of changes to a device state in response to a signal from that device.
   *
   * @param inetAddress The internet address of the device in question.
   * @param changes     The changes to apply to the device state.
   * @param source      The optional source of the event that generated the changes.
   * @return The old and new states of the device in question.
   */
  private def applyChanges(
    inetAddress: InetAddress,
    changes: Changes,
    source: Option[Source]
  ): UIO[State] = for
    (oldState, newState) <- devices.modify(_.applyChanges(inetAddress, changes))
    _ <-
      val newLight = newState.toLight
      if newLight != oldState.toLight then events.publish(newLight -> source).ignore
      else ZIO.unit
  yield newState

  /**
   * Called when a device has sent an error response.
   *
   * @param method      The method that the error pertains to.
   * @param inetAddress The internet address of the device.
   * @param id          The correlation ID of the incoming response.
   * @param error       The payload of the error response.
   */
  private def onErrorResponse(
    method: String,
    inetAddress: InetAddress,
    id: Option[Int],
    error: ErrorResponsePayload
  ): UIO[Unit] = for
    macAddress <- resolve(inetAddress, id)
    _ <- macAddress.fold(ZIO.unit)(mac => completeInvocation(id, mac) *> touch(mac))
    _ <- (error.code, error.message) match
      case (Some(code), Some(message)) => ZIO.logWarning(s"Received $method response error $code: $message")
      case (Some(code), None) => ZIO.logWarning(s"Received $method response error $code.")
      case (None, Some(message)) => ZIO.logWarning(s"Received $method response error: $message")
      case (None, None) => ZIO.logWarning(s"Received $method response error.")
  yield ()

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
      service <- make(
        configuration.discoveryInterval,
        configuration.registrationTtl,
        configuration.purgeTimeout,
        configuration.retryBackoffs,
        clock,
        network
      )
    yield service
  }

  /**
   * Creates a live lighting service.
   *
   * @param discoveryInterval The frequency of broadcast discovery attempts.
   * @param purgeTimeout      The timeout for devices that do not respond.
   * @param retryBackoffs     The durations to wait between retrying outgoing messages.
   * @param clock             The clock service.
   * @param network           The network service.
   * @return A live lighting service.
   */
  def make(
    discoveryInterval: FiniteDuration,
    registrationTtl: FiniteDuration,
    purgeTimeout: FiniteDuration,
    retryBackoffs: List[FiniteDuration],
    clock: Clock,
    network: Network
  ): URIO[Scope, Lighting] = for
    hub <- Hub.unbounded[(Light, Option[Source])]
    devices <- Ref.Synchronized.make(Devices())
    invocations <- ConcurrentMap.make[Int, Invocation]()
    service = LightingLive(
      Duration.fromScala(registrationTtl),
      retryBackoffs.map(Duration.fromScala),
      Duration.fromScala(purgeTimeout),
      clock,
      network,
      hub,
      devices,
      invocations
    )
    _ <- network.subscribe.flatMap(_.mapZIOParUnordered(16)(service.onMessage).runDrain.forkScoped)
    _ <- discovery(network, discoveryInterval)
    _ <- service.register.repeat(Schedule.fixed(Duration.fromScala(registrationTtl / 2))).forkScoped
    _ <- service.purge.repeat(Schedule.fixed(Duration.fromScala(discoveryInterval * Math.PI))).forkScoped
  yield service

  /**
   * Performs scoped discovery broadcasts at the specified interval.
   *
   * @param network The network to broadcast on.
   * @param discoveryInterval The delay between subsequent discovery broadcasts.
   */
  private def discovery(network: Network, discoveryInterval: FiniteDuration): URIO[Scope, Unit] =
    val getPilotRequest = Outgoing.Broadcast(GetPilotRequest())
    val getSystemConfigRequest = Outgoing.Broadcast(GetSystemConfigRequest())
    val repeat = Duration.fromScala(discoveryInterval)
    for
      _ <- network.publish(getPilotRequest)
      _ <- network.publish(getSystemConfigRequest)
      _ <- network.publish(getPilotRequest).repeat(Schedule.fixed(repeat)).delay(repeat).forkScoped
    yield ()
