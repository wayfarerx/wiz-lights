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
package service

import zio.{Scope, UIO, ZIO}
import zio.test.*

import model.{Address, Location}

/**
 * Specifications for the non-abstract elements of the Lighting service.
 */
object LightingSpec extends ZIOSpecDefault:

  private class TestVisitor extends Lighting.Key.Visitor[String]:

    override def onName(name: String): UIO[Option[String]] =
      ZIO.succeed(Some(s"name:$name"))

    override def onNames(names: Iterable[String]): UIO[List[String]] =
      ZIO.succeed(names.map(name => s"name:$name").toList)

    override def onAddress(macAddress: Address): UIO[Option[String]] =
      ZIO.succeed(Some(s"address:$macAddress"))

    override def onAddresses(macAddresses: Iterable[Address]): UIO[List[String]] =
      ZIO.succeed(macAddresses.map(address => s"address:$address").toList)

    override def onLocation(location: Location): UIO[Option[String]] =
      ZIO.succeed(Some(s"location:${location.name}"))

    override def onLocations(locations: Iterable[Location]): UIO[List[String]] =
      ZIO.succeed(locations.map(location => s"location:${location.name}").toList)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("LightingSpec")(
    test("Lighting.Key given instances and singleton objects") {
      assertTrue(
        summon[Lighting.Key[String]] == Lighting.Key.Names,
        summon[Lighting.Key[Address]] == Lighting.Key.Addresses,
        summon[Lighting.Key[Location]] == Lighting.Key.Locations
      )
    },
    test("Lighting.Key.Names dispatching to visitor") {
      val visitor = new TestVisitor
      for
        single <- Lighting.Key.Names("lamp", visitor)
        multiple <- Lighting.Key.Names(List("lamp1", "lamp2"), visitor)
      yield assertTrue(
        single.contains("name:lamp"),
        multiple == List("name:lamp1", "name:lamp2")
      )
    },
    test("Lighting.Key.Addresses dispatching to visitor") {
      val visitor = new TestVisitor
      for
        address1 <- Address.make("0123456789ab")
        address2 <- Address.make("cdef01234567")
        single <- Lighting.Key.Addresses(address1, visitor)
        multiple <- Lighting.Key.Addresses(List(address1, address2), visitor)
      yield assertTrue(
        single.contains("address:0123456789ab"),
        multiple == List("address:0123456789ab", "address:cdef01234567")
      )
    },
    test("Lighting.Key.Locations dispatching to visitor") {
      val visitor = new TestVisitor
      for
        address1 <- Address.make("0123456789ab")
        address2 <- Address.make("cdef01234567")
        location1 <- Location.make("Living Room", address1)
        location2 <- Location.make("Bedroom", address2)
        single <- Lighting.Key.Locations(location1, visitor)
        multiple <- Lighting.Key.Locations(List(location1, location2), visitor)
      yield assertTrue(
        single.contains("location:Living Room"),
        multiple == List("location:Living Room", "location:Bedroom")
      )
    },
    test("Lighting.Key.Visitor apply delegation for single keys") {
      val visitor = new TestVisitor
      for
        address <- Address.make("0123456789ab")
        location <- Location.make("Kitchen", address)
        byName <- visitor("lamp")
        byAddress <- visitor(address)
        byLocation <- visitor(location)
      yield assertTrue(
        byName.contains("name:lamp"),
        byAddress.contains("address:0123456789ab"),
        byLocation.contains("location:Kitchen")
      )
    },
    test("Lighting.Key.Visitor apply delegation for collections of keys") {
      val visitor = new TestVisitor
      for
        address1 <- Address.make("000000000001")
        address2 <- Address.make("000000000002")
        location1 <- Location.make("Kitchen", address1)
        location2 <- Location.make("Living Room", address2)
        byNames <- visitor(List("lamp1", "lamp2"))
        byAddresses <- visitor(List(address1, address2))
        byLocations <- visitor(List(location1, location2))
      yield assertTrue(
        byNames == List("name:lamp1", "name:lamp2"),
        byAddresses == List("address:000000000001", "address:000000000002"),
        byLocations == List("location:Kitchen", "location:Living Room")
      )
    }
  )
