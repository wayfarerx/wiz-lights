package net.wayfarerx.wizlights
package model

/**
 * A location that a light is expected to be found at.
 *
 * @param name       The name of this location.
 * @param macAddress The MAC address of the light at this location.
 */
case class Location(name: String, macAddress: String)
