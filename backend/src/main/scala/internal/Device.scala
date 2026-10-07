package net.wayfarerx.wizlights
package backend
package internal

import java.net.InetAddress
import java.util.concurrent.atomic.AtomicReference

import zio.{Ref, UIO, ZIO}

import model.*

final class Device(val location: Location):
  
  private val _state: AtomicReference[Option[Device.State]] = AtomicReference(None)
  
  def name: String = location.name
  
  def macAddress: Address = location.macAddress
  
  def state: Option[Device.State] = _state.get
  
  def inetAddress: Option[InetAddress] = state.map(_.inetAddress)
  
  def status: Option[Status] = state.map(_.status)
  
  def toLight: UIO[Light] = ZIO.succeed(Light(location, status))
  
object Device:
  
  case class State(inetAddress: InetAddress, status: Status)
