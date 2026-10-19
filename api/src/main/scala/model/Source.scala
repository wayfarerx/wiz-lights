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

import cats.kernel.Order

import zio.{Task, ZIO}

/** An update source for a light. */
opaque type Source <: String = String

/**
 * Factory for update sources.
 */
object Source:

  /** The heartbeat update source. */
  val Heartbeat: Source = "hb"

  /** The UDP command update source. */
  val Udp: Source = "udp"

  /** The mobile application update source. */
  val App: Source = "wfa"

  /** The remote control update source. */
  val Remote: Source = "wzm"

  /** The motion sensor update source. */
  val Sensor: Source = "pir"

  /** The ordering of update sources. */
  given Ordering[Source] = Ordering.String

  /** The cats order of update sources. */
  given Order[Source] = Order.fromOrdering

  /**
   * Creates an update source from the specified string.
   *
   * @param string The string to validate as an update source.
   * @return The validated update source.
   */
  def make(string: String): Task[Source] = string.trim match
    case invalid if invalid.isEmpty => ZIO.fail(IllegalArgumentException("Empty or blank source."))
    case valid => ZIO.succeed(valid)
