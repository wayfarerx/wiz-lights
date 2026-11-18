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
package network

import java.net.InetAddress

/**
 * Base type for changes to the local network interfaces between versions of the network topology.
 *
 * A change to the IP address or subnet of an interface is reported as the deactivation of the old interface followed
 * by the activation of the new one.
 */
sealed trait Event:

  /** The MAC address of the affected interface in hexadecimal. */
  def macAddress: String

  /** The IP address of the affected interface. */
  def ipAddress: InetAddress

  /** The subnet prefix length of the affected interface. */
  def prefixLength: Int

/**
 * Definitions of the supported network topology events.
 */
object Event:

  /**
   * An interface that has become available, so devices on its subnet should be rediscovered and registered with.
   *
   * @param macAddress   The MAC address of the activated interface in hexadecimal.
   * @param ipAddress    The IP address of the activated interface.
   * @param prefixLength The subnet prefix length of the activated interface.
   */
  case class Activated(macAddress: String, ipAddress: InetAddress, prefixLength: Int) extends Event

  /**
   * An interface that is no longer available, so devices reached through it should be marked offline.
   *
   * @param macAddress   The MAC address of the deactivated interface in hexadecimal.
   * @param ipAddress    The IP address of the deactivated interface.
   * @param prefixLength The subnet prefix length of the deactivated interface.
   */
  case class Deactivated(macAddress: String, ipAddress: InetAddress, prefixLength: Int) extends Event
