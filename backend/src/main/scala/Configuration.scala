package net.wayfarerx.wizlights
package backend

import cats.data.NonEmptyList
import model.Locations

import scala.concurrent.duration.FiniteDuration

case class Configuration(
  locations: Locations,
  networkPort: Int,
  sendRetries: NonEmptyList[FiniteDuration]
)
