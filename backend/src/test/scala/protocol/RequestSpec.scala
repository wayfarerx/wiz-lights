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

import model.*

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
        request.method == GetPilotMethod,
        request.withId(Some(99)) == GetPilotRequest(Some(99)),
        requestWithId.withId(None) == GetPilotRequest(None),
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
        request.method == GetSystemConfigMethod,
        request.withId(Some(99)) == GetSystemConfigRequest(Some(99)),
        requestWithId.withId(None) == GetSystemConfigRequest(None),
        requestWithId.id.contains(42),
        request.asJson == expectedJson,
        (request: Request).asJson == expectedJson,
        requestWithId.asJson == expectedJsonWithId,
        (requestWithId: Request).asJson == expectedJsonWithId
      )
    },
    test("encode RegistrationRequest to JSON") {
      val registrationRequest = RegistrationRequest("0123456789ab", "192.168.1.100")
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
        registrationRequest.method == RegistrationMethod,
        registrationRequest.withId(Some(99)) == RegistrationRequest("0123456789ab", "192.168.1.100", true, Some(99)),
        registrationRequestWithId.withId(None) == RegistrationRequest("0123456789ab", "192.168.1.100", true, None),
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
    test("construct RegistrationRequest with domain types and InetAddress") {
      val mac = MacAddress.from("0123456789ab").toOption.get
      val ip = IpAddress.from("192.168.1.100").toOption.get
      val inet = java.net.InetAddress.getByName("192.168.1.100")

      assertTrue(
        RegistrationRequest(mac, ip) == RegistrationRequest("0123456789ab", "192.168.1.100"),
        RegistrationRequest(mac, inet) == RegistrationRequest("0123456789ab", "192.168.1.100"),
        RegistrationRequest(mac, inet, false) == RegistrationRequest("0123456789ab", "192.168.1.100", false),
        RegistrationRequest(mac, inet, true, Some(42)) == RegistrationRequest("0123456789ab", "192.168.1.100", true, Some(42))
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
        stateOnlyRequest.method == SetPilotMethod,
        stateOnlyRequest.withId(Some(99)) == SetPilotRequest(state = Some(true), id = Some(99)),
        idRequest.withId(None) == SetPilotRequest(state = Some(true), id = None),
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
    },
    test("create SetPilotRequest from Status and Update") {
      val disabled = Status.Disabled
      val white = Status.White(Temperature.Warmest, Brightness.Brightest)
      val staticScene = Status.Defined.Static(Scene.Static.Cozy, Brightness.Medium)
      val dynamicScene = Status.Defined.Dynamic(Scene.Dynamic.Party, Speed.Fastest, Brightness.Dimmest)
      val custom = Status.Custom(Color.Red, Brightness.Brightest)

      assertTrue(
        SetPilotRequest.from(disabled) == SetPilotRequest(state = Some(false)),
        SetPilotRequest.from(white) == SetPilotRequest(state = Some(true), temp = Some(Temperature.Warmest), dimming = Some(Brightness.Brightest)),
        SetPilotRequest.from(staticScene) == SetPilotRequest(state = Some(true), sceneId = Some(Scene.Static.Cozy.id), dimming = Some(Brightness.Medium)),
        SetPilotRequest.from(dynamicScene) == SetPilotRequest(state = Some(true), sceneId = Some(Scene.Dynamic.Party.id), speed = Some(Speed.Fastest), dimming = Some(Brightness.Dimmest)),
        SetPilotRequest.from(custom) == SetPilotRequest(state = Some(true), dimming = Some(Brightness.Brightest), r = Some(255), g = Some(0), b = Some(0), c = Some(0), w = Some(0)),
        SetPilotRequest.from(Update.Enabled(true)) == SetPilotRequest(state = Some(true)),
        SetPilotRequest.from(Update.Brightness(Brightness.Medium)) == SetPilotRequest(state = Some(true), dimming = Some(Brightness.Medium)),
        SetPilotRequest.from(Update.Temperature(Temperature.Coolest)) == SetPilotRequest(state = Some(true), temp = Some(Temperature.Coolest)),
        SetPilotRequest.from(Update.Color(Color.Blue)) == SetPilotRequest(state = Some(true), r = Some(0), g = Some(0), b = Some(255), c = Some(0), w = Some(0)),
        SetPilotRequest.from(Update.Scene(Scene.Static.NightLight)) == SetPilotRequest(state = Some(true), sceneId = Some(Scene.Static.NightLight.id)),
        SetPilotRequest.from(Update.Dynamic(Scene.Dynamic.Ocean, Speed.Medium)) == SetPilotRequest(state = Some(true), sceneId = Some(Scene.Dynamic.Ocean.id), speed = Some(Speed.Medium)),
        SetPilotRequest.from(Update.Speed(Speed.Slowest)) == SetPilotRequest(speed = Some(Speed.Slowest)),
        SetPilotRequest.from(Update.Status(white)) == SetPilotRequest(state = Some(true), temp = Some(Temperature.Warmest), dimming = Some(Brightness.Brightest))
      )
    }
  )
