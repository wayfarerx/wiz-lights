package net.wayfarerx.wizlights.backend
package impl

import net.wayfarerx.wizlights.model.{Light, Location}
import zio.{Ref, UIO}

import java.net.InetAddress

private final class Device(val location: Location, status: Ref[Status]) {

  def ipAddress: UIO[Option[InetAddress]] =
    status.get.map(_.current.map(_.ipAddress))

  def toLight: UIO[Option[Light]] =
    status.get.map(_.current.map(Light(location, _))

}
