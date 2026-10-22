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

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}

/**
 * Information about a light on the network.
 *
 * @param inetAddress The internet address of the light.
 * @param signal      The signal strength of the light if available.
 * @param module      The module name of the light if available.
 * @param firmware    The firmware version of the light if available.
 * @param device      The device type ID of the light if available.
 * @param lastSeen    The timestamp when the light was last seen if available.
 * @param home        The home the light belongs to if available.
 * @param room        The room the light is in if available.
 * @param group       The group the light belongs to if defined.
 */
case class Info(
  inetAddress: InetAddress,
  signal: Option[Signal] = None,
  module: Option[Module] = None,
  firmware: Option[Firmware] = None,
  device: Option[Device] = None,
  lastSeen: Option[Instant] = None,
  home: Option[Home] = None,
  room: Option[Room] = None,
  group: Option[Group] = None
)

/**
 * Definitions associated with light information.
 */
object Info:

  /** The circe encoder for internet addresses. */
  given Encoder[InetAddress] = Encoder.encodeString.contramap(_.getHostAddress)

  /** The circe decoder for internet addresses. */
  given Decoder[InetAddress] = Decoder.decodeString.emapTry(str => scala.util.Try(InetAddress.getByName(str)))

  /** The circe encoder for light information. */
  given Encoder[Info] =
    Encoder.forProduct9(
      "inetAddress",
      "signal",
      "module",
      "firmware",
      "device",
      "lastSeen",
      "home",
      "room",
      "group"
    )((info: Info) => (
      info.inetAddress,
      info.signal,
      info.module,
      info.firmware,
      info.device,
      info.lastSeen,
      info.home,
      info.room,
      info.group
    )).mapJsonObject(_.filter(!_._2.isNull))

  /** The circe decoder for light information. */
  given Decoder[Info] = new Decoder[Info]:
    private val productDecoder = Decoder.forProduct9(
      "inetAddress",
      "signal",
      "module",
      "firmware",
      "device",
      "lastSeen",
      "home",
      "room",
      "group"
    )(Info.apply)

    override def decodeAccumulating(cursor: HCursor): ValidatedNel[DecodingFailure, Info] =
      productDecoder.decodeAccumulating(cursor)

    override def apply(cursor: HCursor): Decoder.Result[Info] =
      productDecoder.decodeAccumulating(cursor).toEither.left.map { failures =>
        DecodingFailure(
          DecodingFailure.Reason.CustomReason(failures.map(_.message).toList.mkString(" ")),
          cursor
        )
      }
