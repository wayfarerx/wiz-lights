package net.wayfarerx.wizlights
package backend
package impl

import net.wayfarerx.wizlights.model.Light

/**
 * The status of a device in the network.
 *
 * @param current The current value if it exists.
 * @param invocation The pending invocation if one exists.
 */
case class Status(current: Option[Light], invocation: Option[Invocation])
