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

/**
 * Information about a powered light.
 *
 * @param location The location of this light.
 * @param status   The state of this light if it is powered on.
 */
case class Light(location: Location, status: Option[Status]):

  /** Returns the name of this light. */
  def name: String = location.name

/**
 * Definitions associated with lights.
 */
object Light:

  /** The ordering of lights. */
  given Ordering[Light] = Ordering.by[Light, Location](_.location)

  /** The cats order of lights. */
  given Order[Light] = Order.fromOrdering

