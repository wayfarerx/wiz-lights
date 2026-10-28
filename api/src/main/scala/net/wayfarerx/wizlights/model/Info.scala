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

import cats.data.ValidatedNel

import io.circe.{Decoder, DecodingFailure, Encoder, HCursor}

/**
 * Information about a light on the network.
 *
 * @param ipAddress The IP address of the light.
 * @param signal    The signal strength of the light if available.
 * @param home      The home the light belongs to if available.
 * @param room      The room the light is in if available.
 * @param module    The module name of the light if available.
 * @param firmware  The firmware version of the light if available.
 * @param group     The group the light belongs to if defined.
 * @param device    The device type ID of the light if available.
 */
case class Info(
  ipAddress: IpAddress,
  signal: Option[Signal] = None,
  home: Option[Home] = None,
  room: Option[Room] = None,
  module: Option[Module] = None,
  firmware: Option[Firmware] = None,
  group: Option[Group] = None,
  device: Option[Device] = None
)

/**
 * Definitions associated with light information.
 */
object Info:

  /** The circe encoder for light information. */
  given Encoder[Info] =
    Encoder.forProduct8(
      "ipAddress",
      "signal",
      "home",
      "room",
      "module",
      "firmware",
      "group",
      "device"
    )((info: Info) => (
      info.ipAddress,
      info.signal,
      info.home,
      info.room,
      info.module,
      info.firmware,
      info.group,
      info.device
    )).mapJsonObject(_.filter(!_._2.isNull))

  /** The circe decoder for light information. */
  given Decoder[Info] = new Decoder[Info]:
    private val productDecoder = Decoder.forProduct8(
      "ipAddress",
      "signal",
      "home",
      "room",
      "module",
      "firmware",
      "group",
      "device",
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
