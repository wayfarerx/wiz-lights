package net.wayfarerx.wizlights
package model

/**
 * Scenes a light can perform.
 *
 * @param id   The identifier of this scene.
 * @param name The name of this scene.
 */
enum Scene(val id: Int, val name: String):

  /** The Ocean scene. */
  case Ocean extends Scene(1, "Ocean")

  /** The Romance scene. */
  case Romance extends Scene(2, "Romance")

  /** The Sunset scene. */
  case Sunset extends Scene(3, "Sunset")

  /** The Party scene. */
  case Party extends Scene(4, "Party")

  /** The Fireplace scene. */
  case Fireplace extends Scene(5, "Fireplace")

  /** The Cozy scene. */
  case Cozy extends Scene(6, "Cozy")

  /** The Forest scene. */
  case Forest extends Scene(7, "Forest")

  /** The Pastel Colors scene. */
  case PastelColors extends Scene(8, "Pastel Colors")

  /** The Wake Up scene. */
  case WakeUp extends Scene(9, "Wake Up")

  /** The Bedtime scene. */
  case Bedtime extends Scene(10, "Bedtime")

  /** The Warm White scene. */
  case WarmWhite extends Scene(11, "Warm White")

  /** The Daylight scene. */
  case Daylight extends Scene(12, "Daylight")

  /** The Cool White scene. */
  case CoolWhite extends Scene(13, "Cool White")

  /** The Night Light scene. */
  case NightLight extends Scene(14, "Night Light")

  /** The Focus scene. */
  case Focus extends Scene(15, "Focus")

  /** The Relax scene. */
  case Relax extends Scene(16, "Relax")

  /** The True Colors scene. */
  case TrueColors extends Scene(17, "True Colors")

  /** The TV Time scene. */
  case TVTime extends Scene(18, "TV Time")

  /** The Plant Growth scene. */
  case PlantGrowth extends Scene(19, "Plant Growth")

  /** The Spring scene. */
  case Spring extends Scene(20, "Spring")

  /** The Summer scene. */
  case Summer extends Scene(21, "Summer")

  /** The Fall scene. */
  case Fall extends Scene(22, "Fall")

  /** The Deep Dive scene. */
  case DeepDive extends Scene(23, "Deep Dive")

  /** The Jungle scene. */
  case Jungle extends Scene(24, "Jungle")

  /** The Mojito scene. */
  case Mojito extends Scene(25, "Mojito")

  /** The Club scene. */
  case Club extends Scene(26, "Club")

  /** The Christmas scene. */
  case Christmas extends Scene(27, "Christmas")

  /** The Halloween scene. */
  case Halloween extends Scene(28, "Halloween")

  /** The Candlelight scene. */
  case Candlelight extends Scene(29, "Candlelight")

  /** The Golden White scene. */
  case GoldenWhite extends Scene(30, "Golden White")

  /** The Pulse scene. */
  case Pulse extends Scene(31, "Pulse")

  /** The Steampunk scene. */
  case Steampunk extends Scene(32, "Steampunk")

  /** The Diwali scene. */
  case Diwali extends Scene(33, "Diwali")

  /** The White scene. */
  case White extends Scene(34, "White")

  /** The Alarm scene. */
  case Alarm extends Scene(35, "Alarm")

  /** The Snowy Sky scene. */
  case SnowySky extends Scene(36, "Snowy Sky")

  /** The Rhythm scene. */
  case Rhythm extends Scene(1000, "Rhythm")

/**
 * Definitions associated with scenes.
 */
object Scene:

  /** The scenes indexed by their IDs. */
  private lazy val byId = values.map(s => s.id -> s).toMap

  /** The scenes indexed by their names. */
  private lazy val byName = values.map(s => s.name -> s).toMap

  /**
   * Returns the scene with the specified ID.
   *
   * @param id The ID of the scene to return.
   * @return The scene with the specified ID if it exists.
   */
  def valueBy(id: Int): Option[Scene] = byId.get(id)

  /**
   * Returns the scene with the specified name.
   *
   * @param name The name of the scene to return.
   * @return The scene with the specified name if it exists.
   */
  def valueBy(name: String): Option[Scene] = byName.get(name)
