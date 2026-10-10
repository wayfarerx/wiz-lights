package net.wayfarerx.wizlights
package backend
package internal

import java.net.InetAddress

import model.{Location, Status}

sealed trait Event:
  
  def location: Location

object Event:
  
  case class InetAddressChanged(
    location: Location,
    oldAddress: Option[InetAddress],
    newAddress: Option[InetAddress]
  ) extends Event
  
  case class StatusChanged(
    location: Location,
    status: Option[Status]
  ) extends Event
