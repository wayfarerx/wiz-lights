package net.wayfarerx.wizlights
package backend
package internal

import java.net.InetAddress

import net.wayfarerx.wizlights.backend.network.Incoming
import net.wayfarerx.wizlights.backend.protocol.{GetPilotResponse, SetPilotResponse}
import net.wayfarerx.wizlights.model.*
import zio.{Clock, Ref, Task, UIO, ZIO}

/**
 * Represents the state of a single device on the network.
 *
 * @param location The location of this device.
 * @param clock    The clock to use for telling time.
 * @param _state   The variable that holds the current state of this device.
 */
private final class Device private(
  val location: Location,
  clock: Clock,
  _state: Ref.Synchronized[Option[Device.State]]
):

  /** The name of this device. */
  def name: String = location.name

  /** The MAC address of this device. */
  def macAddress: Address = location.macAddress

  /** The current IP address of this device. */
  def inetAddress: UIO[Option[InetAddress]] = _state.get.map(_.map(_.inetAddress))

  /** Converts the current state of this device into a light. */
  def toLight: UIO[Light] = _state.get.map(s => Light(location, s.map(_.status)))

  /**
   * Notifies this device that "setPilot" has been invoked.
   *
   * @param invocation The invocation that tracks the "setPilot" call.
   * @return The IP address the invocation should be dispatched to.
   */
  def invoking(invocation: Invocation): UIO[Option[InetAddress]] = for
    (inetAddress, toComplete) <- _state.modify {
      case Some(state) =>
        (Some(state.inetAddress) -> state.invoking) -> Some(state.copy(invoking = Some(invocation)))
      case None =>
        (None -> Some(invocation)) -> None
    }
    result <- toComplete.fold(ZIO.succeed(inetAddress))(_.complete(location).map(_ => inetAddress))
  yield result

  /**
   * Notifies this device that an incoming message has been received.
   *
   * @param message The message that was received.
   * @param publish The function to publish events with.
   */
  def received(message: Incoming, publish: Event => UIO[Unit]): Task[Unit] =
    message.response match
      case getPilot: GetPilotResponse =>
        for
          status <- Device.asStatus(getPilot)
          now <- clock.nanoTime
          (changed, status) <- _state.modify { oldState =>
            val newState = oldState match
              case Some(state) => state.copy(message.address, status, now)
              case None => Device.State(message.address, status, now, None)
            (!oldState.map(_.status).contains(newState.status) -> newState.status) -> Some(newState)
          }
          //_ <- if changed then publish(location, Some(message.address -> status)) else ZIO.unit
        yield ()
      case setPilot: SetPilotResponse =>
        for
          now <- clock.nanoTime
          completed <- _state.modifySome(Option.empty[Invocation]) { case Some(state) =>
            state.invoking match
              case Some(invocation) =>
                Some(invocation) -> Some(Device.State(message.address, invocation.outcome, now, None))
              case None =>
                None -> None
          }
          _ <- completed.fold(ZIO.unit) { invocation =>
            for
              _ <- invocation.complete(location)
            //  _ <- publish(location, Some(message.address -> invocation.outcome))
            yield ()
          }
        yield ()

  private def onGetPilotResponse(
    inetAddress: InetAddress,
    response: GetPilotResponse,
    publisher: Event => UIO[Unit]
  ): Task[Unit] =
    for
      status <- Device.asStatus(response)
      now <- clock.nanoTime
      (changed, status) <- _state.modify { oldState =>
        val newState = oldState match
          case Some(state) => state.copy(inetAddress, status, now)
          case None => Device.State(inetAddress, status, now, None)
        (!oldState.map(_.status).contains(newState.status) -> newState.status) -> Some(newState)
      }
      //_ <- if changed then publish(location, Some(message.address -> status)) else ZIO.unit
    yield ()

  private def onSetPilotResponse(
    inetAddress: InetAddress,
    response: SetPilotResponse,
    publisher: Event => UIO[Unit]
  ): Task[Unit] =

    ???

  private def modifyState(
    f: Option[Device.State] => Option[Device.State]
  ): UIO[(Option[Device.State], Option[Device.State])] = {
    _state.modify { oldState =>
      val newState = f(oldState)
      (oldState -> newState) -> newState
    }
  }

private object Device:

  type Publisher = Event => UIO[Unit]

  def make(location: Location, clock: Clock): UIO[Device] = for
    state <- Ref.Synchronized.make[Option[State]](None)
  yield Device(location, clock, state)

  /**
   * Converts a "getPilot" response into a status.
   *
   * @param response The "getPilot" response to convert.
   * @return The resulting status.
   */
  private def asStatus(response: GetPilotResponse): Task[Status] =
    if !response.state then ZIO.succeed(Status.Disabled) else
      for
        brightness <- response.dimming.fold(ZIO.succeed(Brightness.Medium))(Brightness.make)
        result <- response.temp match
          case Some(temp) =>
            for
              temperature <- Temperature.make(temp)
            yield Status.White(temperature, brightness)
          case None =>
            response.sceneId match
              case Some(sceneId) =>
                for
                  scene <- Scene.make(sceneId)
                yield Status.Defined(scene, brightness)
              case None =>
                for
                  color <- Color.make(response.r.getOrElse(0), response.g.getOrElse(0), response.b.getOrElse(0))
                yield Status.Custom(color, brightness)
      yield result

  private case class State(
    inetAddress: InetAddress,
    status: Status,
    lastReceivedAt: Long,
    invoking: Option[Invocation]
  )
