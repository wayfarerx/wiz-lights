package net.wayfarerx.wizlights
package backend

import net.wayfarerx.wizlights.model.Locations

case class Configuration(
  locations: Locations,
  networkPort: Int,
)
