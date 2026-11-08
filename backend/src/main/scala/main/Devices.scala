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

import java.net.InetAddress

import model.*

/**
 * A bidirectional index of known devices.
 *
 * @param byMac The known devices by MAC address.
 * @param byIp  The MAC addresses indexed by current IP address.
 */
private case class Devices(
  byMac: Map[MacAddress, State] = Map.empty,
  byIp: Map[InetAddress, MacAddress] = Map.empty
):

  /** Returns the MAC addresses of all known devices. */
  def keys: collection.Set[MacAddress] = byMac.keySet

  /** Returns the states of all known devices. */
  def values: collection.Iterable[State] = byMac.values

  /** Returns the state of the device with the specified MAC address if known. */
  def get(macAddress: MacAddress): Option[State] = byMac.get(macAddress)

  /** Returns the MAC address of the device with the specified IP address if known. */
  def get(inetAddress: InetAddress): Option[MacAddress] = byIp.get(inetAddress)

  /**
   * Looks up the internet addresses of the devices with the specified MAC addresses.
   *
   * @param macAddresses The MAC addresses of the devices to look up.
   * @return The internet addresses of the devices with the specified MAC addresses.
   */
  def lookup(macAddresses: Iterable[MacAddress]): Map[MacAddress, InetAddress] =
    macAddresses.foldLeft(Map.empty) { (result, macAddress) =>
      byMac.get(macAddress).flatMap(_.inetAddress).fold(result)(inetAddress => result + (macAddress -> inetAddress))
    }

  /**
   * Touches the device with the specified MAC address.
   *
   * @param macAddress The MAC address of the device being touched.
   * @param now        The current time.
   * @return The updated devices index.
   */
  def touch(macAddress: MacAddress, now: Long): Devices =
    byMac.get(macAddress).fold(this) { old =>
      copy(byMac = byMac.updated(macAddress, old.copy(lastSeen = Some(old.lastSeen.fold(now)(Math.max(_, now))))))
    }

  /**
   * Clears the registration timestamp of the device with the specified MAC address.
   *
   * @param macAddress The MAC address of the device being unregistered.
   * @return The updated devices index.
   */
  def unregister(macAddress: MacAddress): Devices =
    byMac.get(macAddress).fold(this) { old =>
      copy(byMac = byMac.updated(macAddress, old.copy(registration = None)))
    }

  /**
   * Clears the authoritative guard for the device with the specified MAC address.
   *
   * @param macAddress The MAC address of the device.
   * @return The updated devices index.
   */
  def clearAuthoritative(macAddress: MacAddress): Devices =
    byMac.get(macAddress).fold(this) { old =>
      copy(byMac = byMac.updated(macAddress, old.copy(authoritativeUntil = None)))
    }

  /**
   * Applies changes to a device state.
   *
   * @param inetAddress The internet address of the device.
   * @param changes     The changes to apply.
   * @return The old and new states and the updated devices index.
   */
  def applyChanges(inetAddress: InetAddress, changes: Changes): ((State, State), Devices) =
    val oldState = byMac.getOrElse(changes.macAddress, State(changes.macAddress))
    val newState = oldState.merge(inetAddress, changes)
    val cleanedByMac = newState.inetAddress match
      case Some(newIp) =>
        byMac.view.mapValues { dev =>
          if dev.macAddress != changes.macAddress && dev.inetAddress.contains(newIp) then
            dev.copy(inetAddress = None)
          else dev
        }.toMap
      case None =>
        byMac
    val updatedByMac = cleanedByMac.updated(changes.macAddress, newState)
    val withoutOldIp = oldState.inetAddress match
      case Some(oldIp) if byIp.get(oldIp).contains(changes.macAddress) =>
        byIp - oldIp
      case _ =>
        byIp
    val updatedByIp = newState.inetAddress match
      case Some(newIp) =>
        withoutOldIp + (newIp -> changes.macAddress)
      case None =>
        withoutOldIp
    (oldState -> newState, Devices(updatedByMac, updatedByIp))

  /**
   * Purges expired devices.
   *
   * @param cutoff The timestamp before which devices are expired.
   * @return The expired devices and the updated devices index.
   */
  def purge(cutoff: Long): (List[State], Devices) =
    val (surviving, expired) = byMac.partition { case (_, state) =>
      state.lastSeen.fold(false)(_ >= cutoff)
    }
    val expiredMacs = expired.keySet
    val updatedByIp = byIp.filterNot { case (_, mac) => expiredMacs.contains(mac) }
    (expired.values.toList, Devices(surviving, updatedByIp))
