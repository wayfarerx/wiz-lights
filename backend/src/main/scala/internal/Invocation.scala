package net.wayfarerx.wizlights
package backend
package internal

import java.net.InetAddress

import net.wayfarerx.wizlights.model.{Location, Status}
import zio.UIO

trait Invocation:

  def outcome: Status
  
  def complete(location: Location): UIO[Unit]
