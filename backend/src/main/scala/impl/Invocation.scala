package net.wayfarerx.wizlights
package backend
package impl

import net.wayfarerx.wizlights.model.Light

/**
 * Represents an invocation of a method on a device.
 *
 * @param id The ID of this invocation.
 * @param state The state this invocation wants to set.
 */
private case class Invocation(id: Long, state: Light.State)
