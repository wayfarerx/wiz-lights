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

import java.net.InetAddress
import java.time.Instant

import cats.data.ValidatedNel
import cats.kernel.Order

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}

/**
 * A light that is on the network.
 *
 * @param address The MAC address of this light.
 * @param status  The status of this light if available.
 * @param info    The information about this light if available.
 */
case class Light(
  address: Address,
  status: Option[Status],
  info: Option[Info]
):

  /** The internet address of this light. */
  def inetAddress: Option[InetAddress] = info.map(_.inetAddress)

  /** The signal strength of this light. */
  def signal: Option[Signal] = info.flatMap(_.signal)

  /** The module name of this light. */
  def module: Option[Module] = info.flatMap(_.module)

  /** The firmware version of this light. */
  def firmware: Option[Firmware] = info.flatMap(_.firmware)

  /** The device type ID of this light. */
  def device: Option[Device] = info.flatMap(_.device)

  /** The timestamp when this light was last seen. */
  def lastSeen: Option[Instant] = info.flatMap(_.lastSeen)

  /** The home the light belongs to. */
  def home: Option[Home] = info.flatMap(_.home)

  /** The room the light is in. */
  def room: Option[Room] = info.flatMap(_.room)

  /** The group the light belongs to if defined. */
  def group: Option[Group] = info.flatMap(_.group)

/**
 * Definitions associated with lights.
 */
object Light:

  /** The ordering of lights. */
  given Ordering[Light] = Ordering.by[Light, String](_.address)

  /** The cats order of lights. */
  given Order[Light] = Order.fromOrdering

  /** The circe encoder for lights. */
  given Encoder[Light] =
    Encoder.forProduct3("address", "status", "info")((l: Light) => (l.address, l.status, l.info))
      .mapJsonObject(_.filter(!_._2.isNull))

  /** The circe decoder for lights. */
  given Decoder[Light] = new Decoder[Light]:

    private val productDecoder = Decoder.forProduct3("address", "status", "info")(Light.apply)

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Light] =
      productDecoder.decodeAccumulating(cursor)

    override def apply(cursor: HCursor): Decoder.Result[Light] =
      productDecoder.decodeAccumulating(cursor).toEither.left.map { failures =>
        DecodingFailure(
          DecodingFailure.Reason.CustomReason(failures.map(_.message).toList.mkString(" ")),
          cursor
        )
      }

