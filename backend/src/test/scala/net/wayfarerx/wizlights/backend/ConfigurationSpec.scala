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

import java.net.InetAddress
import scala.concurrent.duration.*

import zio.Scope
import zio.test.*
import zio.test.Assertion.*

/**
 * Specifications for backend Configuration.
 */
object ConfigurationSpec extends ZIOSpecDefault:

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("ConfigurationSpec")(
    test("create default configuration with make") {
      for
        config <- Configuration.make()
      yield assertTrue(
        config.outgoingWindow == Configuration.DefaultOutgoingWindow,
        config.incomingWindow == Configuration.DefaultIncomingWindow,
        config.commandPort == Configuration.DefaultCommandPort,
        config.listenerPort == Configuration.DefaultListenerPort,
        config.discoveryInterval == Configuration.DefaultDiscoveryInterval,
        config.registrationTtl == Configuration.DefaultRegistrationTtl,
        config.purgeTimeout == Configuration.DefaultPurgeTimeout,
        config.retryBackoffs == Configuration.DefaultRetryBackoffs,
        config.knownDevices == Configuration.DefaultKnownDevices
      )
    },
    test("create custom configuration with make") {
      val customDevices = List(
        InetAddress.getByName("192.168.1.10"),
        InetAddress.getByName("192.168.1.20")
      )
      val customBackoffs = List(100.milliseconds, 250.milliseconds)
      for
        config <- Configuration.make(
          outgoingWindow = 100,
          incomingWindow = 1000,
          commandPort = 12345,
          listenerPort = 12346,
          discoveryInterval = 10.seconds,
          registrationTtl = 2.minutes,
          purgeTimeout = 1.minute,
          retryBackoffs = customBackoffs,
          knownDevices = customDevices
        )
      yield assertTrue(
        config.outgoingWindow == 100,
        config.incomingWindow == 1000,
        config.commandPort == 12345,
        config.listenerPort == 12346,
        config.discoveryInterval == 10.seconds,
        config.registrationTtl == 2.minutes,
        config.purgeTimeout == 1.minute,
        config.retryBackoffs == customBackoffs,
        config.knownDevices == customDevices.toSet,
        config == config,
        config.hashCode == config.hashCode,
        config.toString.contains("Configuration"),
        config match
          case Configuration(ow, iw, cp, lp, di, rt, pt, rb, kd) =>
            ow == 100 && iw == 1000 && cp == 12345 && lp == 12346 &&
              di == 10.seconds && rt == 2.minutes && pt == 1.minute &&
              rb == customBackoffs && kd == customDevices.toSet
      )
    },
    test("accept valid boundary values for ports") {
      for
        minPorts <- Configuration.make(commandPort = 1, listenerPort = 1)
        maxPorts <- Configuration.make(commandPort = 65535, listenerPort = 65535)
      yield assertTrue(
        minPorts.commandPort == 1 && minPorts.listenerPort == 1,
        maxPorts.commandPort == 65535 && maxPorts.listenerPort == 65535
      )
    },
    test("reject invalid command ports") {
      for
        low <- Configuration.make(commandPort = 0).exit
        high <- Configuration.make(commandPort = 65536).exit
      yield assert(low)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(high)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("reject invalid listener ports") {
      for
        low <- Configuration.make(listenerPort = 0).exit
        high <- Configuration.make(listenerPort = 65536).exit
      yield assert(low)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(high)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("reject invalid discovery intervals") {
      for
        zero <- Configuration.make(discoveryInterval = Duration.Zero).exit
        negative <- Configuration.make(discoveryInterval = -1.second).exit
      yield assert(zero)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(negative)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("reject invalid registration TTLs") {
      for
        zero <- Configuration.make(registrationTtl = Duration.Zero).exit
        negative <- Configuration.make(registrationTtl = -1.second).exit
      yield assert(zero)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(negative)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("reject invalid purge timeouts") {
      for
        zero <- Configuration.make(purgeTimeout = Duration.Zero).exit
        negative <- Configuration.make(purgeTimeout = -1.second).exit
      yield assert(zero)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(negative)(fails(isSubtype[IllegalArgumentException](anything)))
    },
    test("support empty retry backoffs") {
      for
        config <- Configuration.make(retryBackoffs = Nil)
      yield assertTrue(config.retryBackoffs.isEmpty)
    },
    test("reject invalid retry backoffs") {
      for
        zero <- Configuration.make(retryBackoffs = List(100.milliseconds, Duration.Zero)).exit
        negative <- Configuration.make(retryBackoffs = List(-5.milliseconds)).exit
      yield assert(zero)(fails(isSubtype[IllegalArgumentException](anything))) &&
        assert(negative)(fails(isSubtype[IllegalArgumentException](anything)))
    }
  )
