package net.wayfarerx.wizlights
package model

import cats.data.NonEmptySet

import scala.collection.immutable.SortedSet

/** A non-empty set of locations. */
type Locations = NonEmptySet[Location]

/** Definitions associated with non-empty sets of locations. */
object Locations:

  /**
   * Creates a non-empty set of locations.
   *
   * @param location  The initial location to include.
   * @param locations The subsequent locations to include.
   * @return A non-empty set of locations.
   */
  def apply(location: Location, locations: Location*): Locations = NonEmptySet.of(location, locations *)

  /**
   * Creates a non-empty set of locations.
   *
   * @param locations The subsequent locations to include.
   * @return A non-empty set of locations if the provided locations are not empty.
   */
  def from(locations: IterableOnce[Location]): Option[Locations] = fromSet(SortedSet.from(locations))

  /**
   * Creates a non-empty set of locations.
   *
   * @param locations The subsequent locations to include.
   * @return A non-empty set of locations if the provided locations are not empty.
   */
  def fromSet(locations: SortedSet[Location]): Option[Locations] = NonEmptySet.fromSet(locations)