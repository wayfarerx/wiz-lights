package net.wayfarerx.wizlights
package backend
package main

import scala.collection.immutable.HashSet

import zio.{Ref, UIO}

import model.*

/**
 * An invocation that tracks the target devices' responses.
 *
 * @param targetsRef The reference to the set of the current targets.
 * @param status The optional status that can complete this invocation.
 */
final class Invocation private(targetsRef: Ref[HashSet[MacAddress]], status: Option[Status]):

  /** Returns the addresses of the devices that this invocation targets. */
  def targets: UIO[Set[MacAddress]] =
    targetsRef.get

  /**
   * Returns true if the specified MAC address is currently targeted.
   *
   * @param macAddress The MAC address to check for targeting.
   */
  def contains(macAddress: MacAddress): UIO[Boolean] =
    targetsRef.get.map(_.contains(macAddress))

  /**
   * Completes this invocation for the specified MAC address.
   *
   * @param macAddress The MAC address to remove from this invocation.
   * @return The optional status associated with this invocation.
   */
  def complete(macAddress: MacAddress): UIO[Option[Status]] =
    targetsRef.modify { oldTargets =>
      val newTargets = oldTargets - macAddress
      (if oldTargets.size == newTargets.size then None else status) -> newTargets
    }

/**
 * Factory for invocations.
 */
object Invocation:

  /**
   * Creates a new invocation.
   *
   * @param macAddresses The MAC addresses of the devices that are being invoked.
   * @param status The optional status associated with the invocation.
   * @return A new invocation.
   */
  def make(macAddresses: Iterable[MacAddress], status: Option[Status] = None): UIO[Invocation] = for
    targets <- Ref.make(HashSet.from(macAddresses))
  yield Invocation(targets, status)
