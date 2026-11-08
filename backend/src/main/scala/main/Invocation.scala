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
package backend
package main

import java.util.concurrent.atomic.AtomicInteger

import zio.{Ref, UIO, ZIO}
import model.*
import service.Update

/**
 * An invocation that tracks the target device's response.
 *
 * @param id        The ID of this invocation.
 * @param createdAt The time this invocation was created at.
 * @param targetRef The reference to the optional target.
 * @param update    The optional update that can complete this invocation.
 */
private final class Invocation private(
  val id: Int,
  val createdAt: Long,
  targetRef: Ref[Option[MacAddress]],
  update: Option[Update]
):

  /** Returns the address of the device that this invocation targets. */
  def target: UIO[Option[MacAddress]] = targetRef.get

  /**
   * Returns true if the specified MAC address is currently targeted.
   *
   * @param macAddress The MAC address to check for targeting.
   */
  def contains(macAddress: MacAddress): UIO[Boolean] = target.map(_.contains(macAddress))

  /**
   * Completes this invocation for the specified MAC address.
   *
   * @param macAddress The MAC address to remove from this invocation.
   * @return The optional update associated with this invocation.
   */
  def complete(macAddress: MacAddress): UIO[Option[Update]] = targetRef.modify {
    case Some(target) if target == macAddress =>
      update -> None
    case other =>
      None -> other
  }

/**
 * Factory for invocations.
 */
private object Invocation:

  /** Generator for invocation IDs. */
  private val ids = AtomicInteger(0)

  /**
   * Creates a new invocation.
   *
   * @param target The MAC address of the device that is being invoked.
   * @param now    The current time.
   * @param update The optional update associated with the invocation.
   * @return A new invocation.
   */
  def make(target: MacAddress, now: Long, update: Option[Update] = None): UIO[Invocation] = for
    id <- nextId
    targetRef <- Ref.make[Option[MacAddress]](Some(target))
  yield Invocation(id, now, targetRef, update)

  /** Generates the next available invocation ID. */
  private def nextId: UIO[Int] =
    ZIO.succeed(ids.incrementAndGet & Int.MaxValue).flatMap {
      case 0 => nextId
      case nonZero => ZIO.succeed(nonZero)
    }
