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
package protocol

import io.circe.parser.decode as decodeJson
import io.circe.DecodingFailure

import zio.Scope
import zio.test.*

/**
 * Specifications for the Response protocol models.
 */
object ResponseSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ResponseSpec")(
    test("decode GetPilotResponse successfully") {
      val jsonString =
        """{
          |  "method": "getPilot",
          |  "env": "pro",
          |  "result": {
          |    "mac": "0123456789ab",
          |    "rssi": -55,
          |    "state": true,
          |    "dimming": 85,
          |    "sceneId": 2,
          |    "speed": 100,
          |    "temp": 3200,
          |    "r": 255,
          |    "g": 200,
          |    "b": 150,
          |    "c": 0,
          |    "w": 0
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[GetPilotResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case GetPilotResponse(Right(payload)) =>
            payload.mac == "0123456789ab" &&
              payload.rssi == -55 &&
              payload.state &&
              payload.dimming.contains(85) &&
              payload.sceneId.contains(2) &&
              payload.speed.contains(100) &&
              payload.temp.contains(3200) &&
              payload.r.contains(255) &&
              payload.g.contains(200) &&
              payload.b.contains(150) &&
              payload.c.contains(0) &&
              payload.w.contains(0)
          case _ => false
      )
    },
    test("decode GetSystemConfigResponse successfully") {
      val jsonString =
        """{
          |  "method": "getSystemConfig",
          |  "env": "pro",
          |  "result": {
          |    "mac": "0123456789ab",
          |    "homeId": 123456,
          |    "roomId": 654321,
          |    "rgn": "us",
          |    "moduleName": "ESP01_SHRGB_03",
          |    "fwVersion": "1.25.0",
          |    "groupId": 0,
          |    "typeId": 1,
          |    "homeLock": false,
          |    "pairingLock": false,
          |    "ping": 0
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[GetSystemConfigResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case GetSystemConfigResponse(Right(payload)) =>
            payload.mac == "0123456789ab" &&
              payload.homeId.contains(123456) &&
              payload.roomId.contains(654321) &&
              payload.rgn.contains("us") &&
              payload.moduleName.contains("ESP01_SHRGB_03") &&
              payload.fwVersion.contains("1.25.0") &&
              payload.groupId.contains(0) &&
              payload.typeId.contains(1) &&
              payload.homeLock.contains(false) &&
              payload.pairingLock.contains(false) &&
              payload.ping.contains(0)
          case _ => false
      )
    },
    test("decode RegistrationResponse successfully") {
      val jsonString =
        """{
          |  "method": "registration",
          |  "env": "pro",
          |  "result": {
          |    "success": true
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[RegistrationResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case RegistrationResponse(Right(payload)) => payload.success
          case _ => false
      )
    },
    test("decode SetPilotResponse successfully") {
      val jsonString =
        """{
          |  "method": "setPilot",
          |  "env": "pro",
          |  "result": {
          |    "success": true
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[SetPilotResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case SetPilotResponse(Right(payload)) => payload.success
          case _ => false
      )
    },
    test("decode FirstBeatResponse using params payload") {
      val jsonString =
        """{
          |  "method": "firstBeat",
          |  "env": "pro",
          |  "params": {
          |    "mac": "0123456789ab",
          |    "homeId": 789012,
          |    "fwVersion": "1.26.0"
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[FirstBeatResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case FirstBeatResponse(Right(payload)) =>
            payload.mac == "0123456789ab" &&
              payload.homeId.contains(789012) &&
              payload.fwVersion.contains("1.26.0") &&
              payload.roomId.isEmpty
          case _ => false
      )
    },
    test("decode SyncPilotResponse using params payload") {
      val jsonString =
        """{
          |  "method": "syncPilot",
          |  "env": "pro",
          |  "params": {
          |    "mac": "0123456789ab",
          |    "rssi": -60,
          |    "state": true,
          |    "src": "udp",
          |    "dimming": 50,
          |    "temp": 2700
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[SyncPilotResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case SyncPilotResponse(Right(payload)) =>
            payload.mac == "0123456789ab" &&
              payload.rssi == -60 &&
              payload.state &&
              payload.src.contains("udp") &&
              payload.dimming.contains(50) &&
              payload.temp.contains(2700) &&
              payload.r.isEmpty
          case _ => false
      )
    },
    test("decode error response payload") {
      val jsonString =
        """{
          |  "method": "setPilot",
          |  "env": "pro",
          |  "error": {
          |    "code": -32602,
          |    "message": "Invalid params"
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      val decodedSpecificResponse = decodeJson[SetPilotResponse](jsonString)

      assertTrue(
        decodedResponse.isRight,
        decodedSpecificResponse.isRight,
        decodedResponse == decodedSpecificResponse,
        decodedResponse.toOption.get match
          case SetPilotResponse(Left(errorPayload)) =>
            errorPayload.code.contains(-32602) &&
              errorPayload.message.contains("Invalid params")
          case _ => false
      )
    },
    test("fail when method is not supported") {
      val jsonString =
        """{
          |  "method": "unsupportedMethod",
          |  "result": {
          |    "success": true
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](jsonString)
      assertTrue(
        decodedResponse.isLeft,
        decodedResponse.left.toOption.get.isInstanceOf[DecodingFailure],
        decodedResponse.left.toOption.get.getMessage.contains(
          "The value \"unsupportedMethod\" is not supported as a value for the \"method\" key."
        )
      )
    },
    test("fail when payload key is missing") {
      val missingPayloadJson =
        """{
          |  "method": "getPilot"
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](missingPayloadJson)
      assertTrue(
        decodedResponse.isLeft,
        decodedResponse.left.toOption.get.isInstanceOf[DecodingFailure],
        decodedResponse.left.toOption.get.getMessage.contains(
          "Either the \"result\" key or the \"error\" key must be present."
        )
      )
    },
    test("fail when both payload and error keys are present") {
      val conflictingKeysJson =
        """{
          |  "method": "getPilot",
          |  "result": {
          |    "mac": "0123456789ab",
          |    "rssi": -50,
          |    "state": true
          |  },
          |  "error": {
          |    "code": 100,
          |    "message": "Conflicting error"
          |  }
          |}""".stripMargin

      val decodedResponse = decodeJson[Response](conflictingKeysJson)
      assertTrue(
        decodedResponse.isLeft,
        decodedResponse.left.toOption.get.isInstanceOf[DecodingFailure],
        decodedResponse.left.toOption.get.getMessage.contains(
          "Only one of the \"result\" key or the \"error\" key may be present."
        )
      )
    }
  )
