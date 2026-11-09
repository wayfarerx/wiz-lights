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

import io.circe.{Decoder, Encoder, Json}

import zio.{Scope, ZIO}
import zio.test.*

/**
 * Specifications for the IpAddress model.
 */
object IpAddressSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("IpAddressSpec")(
    test("validate and construct IP addresses from strings") {
      assertTrue(
        IpAddress.from("0.0.0.0") == Right("0.0.0.0"),
        IpAddress.from("127.0.0.1") == Right("127.0.0.1"),
        IpAddress.from("192.168.1.100") == Right("192.168.1.100"),
        IpAddress.from("255.255.255.255") == Right("255.255.255.255"),
        IpAddress.from("  10.0.0.1  ") == Right("10.0.0.1"),
        IpAddress.from("") == Left("Empty or blank IP address."),
        IpAddress.from("   ") == Left("Empty or blank IP address."),
        IpAddress.from("256.0.0.1") == Left("Malformed IP address: 256.0.0.1."),
        IpAddress.from("192.168.1") == Left("Malformed IP address: 192.168.1."),
        IpAddress.from("192.168.1.1.1") == Left("Malformed IP address: 192.168.1.1.1."),
        IpAddress.from("192.168.a.1") == Left("Malformed IP address: 192.168.a.1."),
        IpAddress.from(".1.2.3.4") == Left("Malformed IP address: .1.2.3.4."),
        IpAddress.from("1.2.3.4.") == Left("Malformed IP address: 1.2.3.4..")
      )
    },
    test("construct IP address effect with make") {
      for
        made <- IpAddress.make("  192.168.1.1  ")
        errBlank <- IpAddress.make("   ").flip
        errMalformed <- IpAddress.make("bad").flip
      yield assertTrue(
        made == "192.168.1.1",
        errBlank.getMessage == "Empty or blank IP address.",
        errMalformed.getMessage == "Malformed IP address: bad."
      )
    },
    test("encode and decode IpAddress as JSON") {
      val ip = IpAddress.from("192.168.1.1").toOption.get
      val encoded = Encoder[IpAddress].apply(ip)
      assertTrue(
        encoded == Json.fromString("192.168.1.1"),
        Decoder[IpAddress].decodeJson(encoded) == Right(ip),
        Decoder[IpAddress].decodeJson(Json.fromString("invalid-ip")).isLeft,
        Decoder[IpAddress].decodeJson(Json.fromInt(12345)).isLeft
      )
    }
  )
