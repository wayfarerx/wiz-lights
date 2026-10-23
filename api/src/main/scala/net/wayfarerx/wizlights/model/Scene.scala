/*
 * Copyright (c) 2026 wayfarerx.net.
 *
 * This file is licensed to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package net.wayfarerx.wizlights
package model

import io.circe.{Decoder, Encoder}

import zio.{IO, ZIO}

/**
 * Scenes a light can perform.
 */
sealed trait Scene:

  /** The identifier of this scene. */
  def id: Int

  /** The name of this scene. */
  def name: String

  /** True if this scene is static, false if it is dynamic. */
  def isStatic: Boolean

  /** True if this scene is dynamic, false if it is static. */
  final def isDynamic: Boolean = !isStatic

/**
 * Definitions associated with scenes.
 */
object Scene:

  /** The circe encoder for scenes. */
  given Encoder[Scene] = Encoder.encodeInt.contramap(_.id)

  /** The circe decoder for scenes. */
  given Decoder[Scene] = Decoder.instance { cursor =>
    if cursor.value.isNumber then Decoder.decodeInt.emap(from)(cursor)
    else Decoder.decodeString.emap(from)(cursor)
  }

  /** Export the static scenes. */
  export Static.{
    Cozy,
    WarmWhite,
    Daylight,
    CoolWhite,
    NightLight,
    Focus,
    Relax,
    TrueColors,
    TVTime,
    PlantGrowth,
    White,
    Rhythm
  }

  /** Export the dynamic scenes. */
  export Dynamic.{
    Ocean,
    Romance,
    Sunset,
    Party,
    Fireplace,
    Forest,
    PastelColors,
    WakeUp,
    Bedtime,
    Spring,
    Summer,
    Fall,
    DeepDive,
    Jungle,
    Mojito,
    Club,
    Christmas,
    Halloween,
    Candlelight,
    GoldenWhite,
    Pulse,
    Steampunk,
    Diwali,
    Alarm,
    SnowySky,
    User1,
    User2,
    User3,
    User4,
    User5,
    User6,
    User7,
    User8,
    User9,
    User10
  }

  /** The scenes indexed by their IDs. */
  val values: List[Scene] =
    (Static.values.iterator ++ Dynamic.values.iterator).toList.sortBy(_.id)

  /** The scenes indexed by their IDs. */
  private lazy val byId = values.map(scene => scene.id -> scene).toMap

  /** The scenes indexed by their names. */
  private lazy val byName = values.map(scene => scene.name.toLowerCase -> scene).toMap

  /**
   * Attempts to return the scene with the specified ID.
   *
   * @param id The ID of the scene to return.
   * @return The scene with the specified ID or a failure.
   */
  def from(id: Int): Either[String, Scene] =
    byId.get(id).toRight(s"Invalid scene ID: $id.")

  /**
   * Returns the scene with the specified ID.
   *
   * @param id The ID of the scene to return.
   * @return The scene with the specified ID if it exists or fails if it does not.
   */
  def make(id: Int): IO[IllegalArgumentException, Scene] =
    ZIO.fromEither(from(id)).mapError(IllegalArgumentException(_))

  /**
   * Attempts to return the scene with the specified name.
   *
   * @param name The name of the scene to return.
   * @return The scene with the specified name or a failure.
   */
  def from(name: String): Either[String, Scene] =
    byName.get(name.toLowerCase.trim).toRight(s"Invalid scene name: $name.")

  /**
   * Returns the scene with the specified name.
   *
   * @param name The name of the scene to return.
   * @return The scene with the specified name if it exists or fails if it does not.
   */
  def make(name: String): IO[IllegalArgumentException, Scene] =
    ZIO.fromEither(from(name)).mapError(IllegalArgumentException(_))

  /**
   * Static scenes that maintain a fixed color or color temperature.
   *
   * @param id   The identifier of this static scene.
   * @param name The name of this static scene.
   */
  enum Static(val id: Int, val name: String) extends Scene:

    /* True because this is a static scene. */
    override def isStatic: Boolean = true

    /** The Cozy scene. */
    case Cozy extends Static(6, "Cozy")

    /** The Warm White scene. */
    case WarmWhite extends Static(11, "Warm White")

    /** The Daylight scene. */
    case Daylight extends Static(12, "Daylight")

    /** The Cool White scene. */
    case CoolWhite extends Static(13, "Cool White")

    /** The Night Light scene. */
    case NightLight extends Static(14, "Night Light")

    /** The Focus scene. */
    case Focus extends Static(15, "Focus")

    /** The Relax scene. */
    case Relax extends Static(16, "Relax")

    /** The True Colors scene. */
    case TrueColors extends Static(17, "True Colors")

    /** The TV Time scene. */
    case TVTime extends Static(18, "TV Time")

    /** The Plant Growth scene. */
    case PlantGrowth extends Static(19, "Plant Growth")

    /** The White scene. */
    case White extends Static(34, "White")

    /** The Rhythm scene. */
    case Rhythm extends Static(1000, "Rhythm")

  /**
   * Definitions associated with static scenes.
   */
  object Static:

    /** The circe encoder for static scenes. */
    given Encoder[Static] = Encoder.encodeInt.contramap(_.id)

    /** The circe decoder for static scenes. */
    given Decoder[Static] = Decoder.instance { cursor =>
      if cursor.value.isNumber then Decoder.decodeInt.emap(from)(cursor)
      else Decoder.decodeString.emap(from)(cursor)
    }

    /** The static scenes indexed by their IDs. */
    private lazy val byId = values.map(scene => scene.id -> scene).toMap

    /** The static scenes indexed by their names. */
    private lazy val byName = values.map(scene => scene.name.toLowerCase -> scene).toMap

    /**
     * Attempts to return the static scene with the specified ID.
     *
     * @param id The ID of the static scene to return.
     * @return The static scene with the specified ID or a failure.
     */
    def from(id: Int): Either[String, Static] =
      byId.get(id).toRight(s"Invalid static scene ID: $id.")

    /**
     * Returns the static scene with the specified ID.
     *
     * @param id The ID of the static scene to return.
     * @return The static scene with the specified ID if it exists or fails if it does not.
     */
    def make(id: Int): IO[IllegalArgumentException, Static] =
      ZIO.fromEither(from(id)).mapError(IllegalArgumentException(_))

    /**
     * Attempts to return the static scene with the specified name.
     *
     * @param name The name of the static scene to return.
     * @return The static scene with the specified name or a failure.
     */
    def from(name: String): Either[String, Static] =
      byName.get(name.toLowerCase.trim).toRight(s"Invalid static scene name: $name.")

    /**
     * Returns the static scene with the specified name.
     *
     * @param name The name of the static scene to return.
     * @return The static scene with the specified name if it exists or fails if it does not.
     */
    def make(name: String): IO[IllegalArgumentException, Static] =
      ZIO.fromEither(from(name)).mapError(IllegalArgumentException(_))

  /**
   * Dynamic scenes that cycle through colors or change over time.
   *
   * @param id   The identifier of this dynamic scene.
   * @param name The name of this dynamic scene.
   */
  enum Dynamic(val id: Int, val name: String) extends Scene:

    /* False because this is a dynamic scene. */
    override def isStatic: Boolean = false

    /** The Ocean scene. */
    case Ocean extends Dynamic(1, "Ocean")

    /** The Romance scene. */
    case Romance extends Dynamic(2, "Romance")

    /** The Sunset scene. */
    case Sunset extends Dynamic(3, "Sunset")

    /** The Party scene. */
    case Party extends Dynamic(4, "Party")

    /** The Fireplace scene. */
    case Fireplace extends Dynamic(5, "Fireplace")

    /** The Forest scene. */
    case Forest extends Dynamic(7, "Forest")

    /** The Pastel Colors scene. */
    case PastelColors extends Dynamic(8, "Pastel Colors")

    /** The Wake-Up scene. */
    case WakeUp extends Dynamic(9, "Wake-Up")

    /** The Bedtime scene. */
    case Bedtime extends Dynamic(10, "Bedtime")

    /** The Spring scene. */
    case Spring extends Dynamic(20, "Spring")

    /** The Summer scene. */
    case Summer extends Dynamic(21, "Summer")

    /** The Fall scene. */
    case Fall extends Dynamic(22, "Fall")

    /** The Deep Dive scene. */
    case DeepDive extends Dynamic(23, "Deep Dive")

    /** The Jungle scene. */
    case Jungle extends Dynamic(24, "Jungle")

    /** The Mojito scene. */
    case Mojito extends Dynamic(25, "Mojito")

    /** The Club scene. */
    case Club extends Dynamic(26, "Club")

    /** The Christmas scene. */
    case Christmas extends Dynamic(27, "Christmas")

    /** The Halloween scene. */
    case Halloween extends Dynamic(28, "Halloween")

    /** The Candlelight scene. */
    case Candlelight extends Dynamic(29, "Candlelight")

    /** The Golden White scene. */
    case GoldenWhite extends Dynamic(30, "Golden White")

    /** The Pulse scene. */
    case Pulse extends Dynamic(31, "Pulse")

    /** The Steampunk scene. */
    case Steampunk extends Dynamic(32, "Steampunk")

    /** The Diwali scene. */
    case Diwali extends Dynamic(33, "Diwali")

    /** The Alarm scene. */
    case Alarm extends Dynamic(35, "Alarm")

    /** The Snowy Sky scene. */
    case SnowySky extends Dynamic(36, "Snowy Sky")

    /** The User 1 scene. */
    case User1 extends Dynamic(256, "User 1")

    /** The User 2 scene. */
    case User2 extends Dynamic(257, "User 2")

    /** The User 3 scene. */
    case User3 extends Dynamic(258, "User 3")

    /** The User 4 scene. */
    case User4 extends Dynamic(259, "User 4")

    /** The User 5 scene. */
    case User5 extends Dynamic(260, "User 5")

    /** The User 6 scene. */
    case User6 extends Dynamic(261, "User 6")

    /** The User 7 scene. */
    case User7 extends Dynamic(262, "User 7")

    /** The User 8 scene. */
    case User8 extends Dynamic(263, "User 8")

    /** The User 9 scene. */
    case User9 extends Dynamic(264, "User 9")

    /** The User 10 scene. */
    case User10 extends Dynamic(265, "User 10")

  /**
   * Definitions associated with dynamic scenes.
   */
  object Dynamic:

    /** The circe encoder for dynamic scenes. */
    given Encoder[Dynamic] = Encoder.encodeInt.contramap(_.id)

    /** The circe decoder for dynamic scenes. */
    given Decoder[Dynamic] = Decoder.instance { cursor =>
      if cursor.value.isNumber then Decoder.decodeInt.emap(from)(cursor)
      else Decoder.decodeString.emap(from)(cursor)
    }

    /** The dynamic scenes indexed by their IDs. */
    private lazy val byId = values.map(scene => scene.id -> scene).toMap

    /** The dynamic scenes indexed by their names. */
    private lazy val byName = values.map(scene => scene.name.toLowerCase -> scene).toMap

    /**
     * Attempts to return the dynamic scene with the specified ID.
     *
     * @param id The ID of the dynamic scene to return.
     * @return The dynamic scene with the specified ID or a failure.
     */
    def from(id: Int): Either[String, Dynamic] =
      byId.get(id).toRight(s"Invalid dynamic scene ID: $id.")

    /**
     * Returns the dynamic scene with the specified ID.
     *
     * @param id The ID of the dynamic scene to return.
     * @return The dynamic scene with the specified ID if it exists or fails if it does not.
     */
    def make(id: Int): IO[IllegalArgumentException, Dynamic] =
      ZIO.fromEither(from(id)).mapError(IllegalArgumentException(_))

    /**
     * Attempts to return the dynamic scene with the specified name.
     *
     * @param name The name of the dynamic scene to return.
     * @return The dynamic scene with the specified name or a failure.
     */
    def from(name: String): Either[String, Dynamic] =
      byName.get(name.toLowerCase.trim).toRight(s"Invalid dynamic scene name: $name.")

    /**
     * Returns the dynamic scene with the specified name.
     *
     * @param name The name of the dynamic scene to return.
     * @return The dynamic scene with the specified name if it exists or fails if it does not.
     */
    def make(name: String): IO[IllegalArgumentException, Dynamic] =
      ZIO.fromEither(from(name)).mapError(IllegalArgumentException(_))
