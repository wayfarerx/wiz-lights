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

import io.circe.Json
import io.circe.syntax.*

import zio.Scope
import zio.test.*

/**
 * Specifications for the Request protocol models.
 */
object RequestSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("RequestSpec")(
    test("encode GetPilotRequest to JSON") {
      val request = GetPilotRequest()
      val requestWithId = GetPilotRequest(Some(42))
      val expectedJson = Json.obj(
        "method" -> Json.fromString("getPilot"),
        "params" -> Json.obj()
      )
      val expectedJsonWithId = Json.obj(
        "id" -> Json.fromInt(42),
        "method" -> Json.fromString("getPilot"),
        "params" -> Json.obj()
      )
      assertTrue(
        request.id.isEmpty,
        requestWithId.id.contains(42),
        request.asJson == expectedJson,
        (request: Request).asJson == expectedJson,
        requestWithId.asJson == expectedJsonWithId,
        (requestWithId: Request).asJson == expectedJsonWithId
      )
    },
    test("encode GetSystemConfigRequest to JSON") {
      val request = GetSystemConfigRequest()
      val requestWithId = GetSystemConfigRequest(Some(42))
      val expectedJson = Json.obj(
        "method" -> Json.fromString("getSystemConfig"),
        "params" -> Json.obj()
      )
      val expectedJsonWithId = Json.obj(
        "id" -> Json.fromInt(42),
        "method" -> Json.fromString("getSystemConfig"),
        "params" -> Json.obj()
      )
      assertTrue(
        request.id.isEmpty,
        requestWithId.id.contains(42),
        request.asJson == expectedJson,
        (request: Request).asJson == expectedJson,
        requestWithId.asJson == expectedJsonWithId,
        (requestWithId: Request).asJson == expectedJsonWithId
      )
    },
    test("encode RegistrationRequest to JSON") {
      val registrationRequest = RegistrationRequest("0123456789ab", "192.168.1.100", true)
      val deregistrationRequest = RegistrationRequest("0123456789ab", "192.168.1.100", false)
      val defaultRegistrationRequest = RegistrationRequest("0123456789ab", "192.168.1.100")
      val registrationRequestWithId = RegistrationRequest("0123456789ab", "192.168.1.100", true, Some(42))

      val expectedRegistrationJson = Json.obj(
        "method" -> Json.fromString("registration"),
        "params" -> Json.obj(
          "phoneMac" -> Json.fromString("0123456789ab"),
          "phoneIp" -> Json.fromString("192.168.1.100"),
          "register" -> Json.fromBoolean(true)
        )
      )
      val expectedDeregistrationJson = Json.obj(
        "method" -> Json.fromString("registration"),
        "params" -> Json.obj(
          "phoneMac" -> Json.fromString("0123456789ab"),
          "phoneIp" -> Json.fromString("192.168.1.100"),
          "register" -> Json.fromBoolean(false)
        )
      )
      val expectedRegistrationWithIdJson = Json.obj(
        "id" -> Json.fromInt(42),
        "method" -> Json.fromString("registration"),
        "params" -> Json.obj(
          "phoneMac" -> Json.fromString("0123456789ab"),
          "phoneIp" -> Json.fromString("192.168.1.100"),
          "register" -> Json.fromBoolean(true)
        )
      )

      assertTrue(
        registrationRequest.phoneMac == "0123456789ab",
        registrationRequest.phoneIp == "192.168.1.100",
        registrationRequest.register,
        registrationRequest.id.isEmpty,
        !deregistrationRequest.register,
        defaultRegistrationRequest.register,
        registrationRequestWithId.id.contains(42),
        registrationRequest.asJson == expectedRegistrationJson,
        (registrationRequest: Request).asJson == expectedRegistrationJson,
        deregistrationRequest.asJson == expectedDeregistrationJson,
        (deregistrationRequest: Request).asJson == expectedDeregistrationJson,
        registrationRequestWithId.asJson == expectedRegistrationWithIdJson,
        (registrationRequestWithId: Request).asJson == expectedRegistrationWithIdJson
      )
    },
    test("encode SetPilotRequest to JSON with partial parameters") {
      val stateOnlyRequest = SetPilotRequest(state = Some(true))
      val expectedStateOnlyJson = Json.obj(
        "method" -> Json.fromString("setPilot"),
        "params" -> Json.obj(
          "state" -> Json.fromBoolean(true)
        )
      )

      val brightnessRequest = SetPilotRequest(
        state = Some(true),
        dimming = Some(75)
      )
      val expectedBrightnessJson = Json.obj(
        "method" -> Json.fromString("setPilot"),
        "params" -> Json.obj(
          "state" -> Json.fromBoolean(true),
          "dimming" -> Json.fromInt(75)
        )
      )

      val idRequest = SetPilotRequest(
        state = Some(true),
        id = Some(42)
      )
      val expectedIdJson = Json.obj(
        "id" -> Json.fromInt(42),
        "method" -> Json.fromString("setPilot"),
        "params" -> Json.obj(
          "state" -> Json.fromBoolean(true)
        )
      )

      assertTrue(
        stateOnlyRequest.id.isEmpty,
        stateOnlyRequest.asJson == expectedStateOnlyJson,
        (stateOnlyRequest: Request).asJson == expectedStateOnlyJson,
        brightnessRequest.asJson == expectedBrightnessJson,
        (brightnessRequest: Request).asJson == expectedBrightnessJson,
        idRequest.id.contains(42),
        idRequest.asJson == expectedIdJson,
        (idRequest: Request).asJson == expectedIdJson
      )
    },
    test("encode SetPilotRequest to JSON with complete parameters") {
      val completeRequest = SetPilotRequest(
        state = Some(true),
        dimming = Some(100),
        sceneId = Some(1),
        speed = Some(150),
        temp = Some(3000),
        r = Some(255),
        g = Some(128),
        b = Some(64),
        c = Some(32),
        w = Some(16),
        id = Some(42)
      )
      val expectedCompleteJson = Json.obj(
        "id" -> Json.fromInt(42),
        "method" -> Json.fromString("setPilot"),
        "params" -> Json.obj(
          "state" -> Json.fromBoolean(true),
          "dimming" -> Json.fromInt(100),
          "sceneId" -> Json.fromInt(1),
          "speed" -> Json.fromInt(150),
          "temp" -> Json.fromInt(3000),
          "r" -> Json.fromInt(255),
          "g" -> Json.fromInt(128),
          "b" -> Json.fromInt(64),
          "c" -> Json.fromInt(32),
          "w" -> Json.fromInt(16)
        )
      )

      assertTrue(
        completeRequest.id.contains(42),
        completeRequest.asJson == expectedCompleteJson,
        (completeRequest: Request).asJson == expectedCompleteJson
      )
    }
  )
