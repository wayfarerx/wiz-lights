package net.wayfarerx.wizlights
package backend

import model.Locations

case class Configuration(
  locations: Locations,
  networkPort: Int,
)
