package net.wayfarerx.wizlights
package service

import cats.data.NonEmptySet

import zio.stream.UStream
import zio.{Scope, UIO, URIO}

import model.{Address, Light, Location, Status}

/**
 * A service that interacts with configured lights.
 */
trait Lighting:

  /** Returns the lights managed by this service. */
  def lights: UIO[NonEmptySet[Light]]

  /**
   * Returns the light with the specified name.
   *
   * @tparam Key The type of key to use.
   * @param key The key of the light to return.
   * @return The light with the specified name.
   */
  def get[Key: Lighting.Key](key: Key): UIO[Option[Light]]

  /**
   * Returns the lights with the specified names.
   *
   * @tparam Key The type of key to use.
   * @param keys The keys of the lights to return.
   * @return The requested lights.
   */
  def getAll[Key: Lighting.Key](keys: Iterable[Key]): UIO[Set[Light]]

  /**
   * Updates the status of the light with the specified name.
   *
   * @tparam Key The type of key to use.
   * @param key    The keys of the light to set the status of.
   * @param status The status to set on the light with the specified name.
   * @return True if a light with the specified name was updated.
   */
  def update[Key: Lighting.Key](key: Key, status: Status): UIO[Boolean]

  /**
   * Updates the status of the lights with the specified names.
   *
   * @tparam Key The type of key to use.
   * @param keys   The keys of the lights to set the status of.
   * @param status The status to set on the lights with the specified names.
   * @return The number of lights with the specified names that were updated.
   */
  def updateAll[Key: Lighting.Key](keys: Iterable[Key], status: Status): UIO[Int]

  /**
   * Subscribes to events from this service.
   *
   * @return A stream of light states published by this service bound to the required scope.
   */
  def subscribe: URIO[Scope, UStream[Light]]

/**
 * Definitions associated with lighting services.
 */
object Lighting:

  /** Represents types that serve as keys for the lighting service. */
  sealed trait Key[T]:

    /**
     * Applies a visitor to a key.
     *
     * @tparam U The type returned by the visitor.
     * @param key The key to visit.
     * @param visitor The visitor to apply.
     * @return The result returned by the visitor.
     */
    def apply[U](key: T, visitor: Key.Visitor[U]): UIO[Option[U]]

    /**
     * Applies a visitor to a collection of keys.
     *
     * @tparam U The type returned by the visitor.
     * @param keys The keys to visit.
     * @param visitor The visitor to apply.
     * @return The result returned by the visitor.
     */
    def apply[U](keys: Iterable[T], visitor: Key.Visitor[U]): UIO[List[U]]

  /**
   * Definitions of the types that serve as keys for the lighting service.
   */
  object Key:

    /** Names can be used as keys for the lighting service. */
    given Key[String] = Names

    /** Addresses can be used as keys for the lighting service. */
    given Key[Address] = Addresses

    /** Locations can be used as keys for the lighting service. */
    given Key[Location] = Locations

    /**
     * Names can be used as keys for the lighting service.
     */
    case object Names extends Key[String]:

      /* Apply a visitor to a key. */
      override def apply[U](key: String, visitor: Visitor[U]): UIO[Option[U]] =
        visitor.onName(key)

      /* Apply a visitor to a collection of keys. */
      override def apply[U](keys: Iterable[String], visitor: Visitor[U]): UIO[List[U]] =
        visitor.onNames(keys)

    /**
     * Addresses can be used as keys for the lighting service.
     */
    case object Addresses extends Key[Address]:

      /* Apply a visitor to a key. */
      override def apply[U](key: Address, visitor: Visitor[U]): UIO[Option[U]] =
        visitor.onAddress(key)

      /* Apply a visitor to a collection of keys. */
      override def apply[U](keys: Iterable[Address], visitor: Visitor[U]): UIO[List[U]] =
        visitor.onAddresses(keys)

    /**
     * Locations can be used as keys for the lighting service.
     */
    case object Locations extends Key[Location]:

      /* Apply a visitor to a key. */
      override def apply[U](key: Location, visitor: Visitor[U]): UIO[Option[U]] =
        visitor.onLocation(key)

      /* Apply a visitor to a collection of keys. */
      override def apply[U](keys: Iterable[Location], visitor: Visitor[U]): UIO[List[U]] =
        visitor.onLocations(keys)

    /**
     * A visitor for key types.
     *
     * @tparam T The type returned by this visitor.
     */
    trait Visitor[T]:

      /**
       * Applies this visitor to the specified key.
       *
       * @tparam Key The type of key apply.
       * @param key The key to apply.
       * @return The result of applying this visitor to the specified key.
       */
      final def apply[Key: Lighting.Key](key: Key): UIO[Option[T]] =
        summon[Lighting.Key[Key]].apply(key, this)

      /**
       * Applies this visitor to the specified keys.
       *
       * @tparam Key The type of key apply.
       * @param keys The keys to apply.
       * @return The result of applying this visitor to the specified keys.
       */
      final def apply[Key: Lighting.Key](keys: Iterable[Key]): UIO[List[T]] =
        summon[Lighting.Key[Key]].apply(keys, this)

      /**
       * Called when the key type is a name.
       *
       * @param name The name to visit.
       * @return The result of this visitor,
       */
      def onName(name: String): UIO[Option[T]]

      /**
       * Called when the key type is a collection of names.
       *
       * @param names The names to visit.
       * @return The result of this visitor,
       */
      def onNames(names: Iterable[String]): UIO[List[T]]

      /**
       * Called when the key type is a MAC address.
       *
       * @param macAddress The MAC address to visit.
       * @return The result of this visitor,
       */
      def onAddress(macAddress: Address): UIO[Option[T]]

      /**
       * Called when the key type is a collection of MAC addresses.
       *
       * @param macAddresses The MAC addresses to visit.
       * @return The result of this visitor,
       */
      def onAddresses(macAddresses: Iterable[Address]): UIO[List[T]]

      /**
       * Called when the key type is a location.
       *
       * @param location The location to visit.
       * @return The result of this visitor,
       */
      def onLocation(location: Location): UIO[Option[T]]

      /**
       * Called when the key type is a collection of locations.
       *
       * @param locations The locations to visit.
       * @return The result of this visitor,
       */
      def onLocations(locations: Iterable[Location]): UIO[List[T]]
