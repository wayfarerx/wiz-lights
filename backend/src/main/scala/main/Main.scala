package net.wayfarerx.wizlights
package backend
package main

import java.net.InetAddress

import io.circe.syntax.*
import zio.{Console, Duration, RIO, ZIO, ZIOAppDefault, ZLayer}
import network.{Network, NetworkLive, Outgoing}
import protocol.SetPilotRequest

object Main extends ZIOAppDefault {

  override def run =
    app.provide(ZLayer(Configuration.make()), NetworkLive.layer)

  def app: RIO[Network, Unit] = ZIO.scoped {
    for
      network <- ZIO.service[Network]
      _ <- ZIO.sleep(Duration.fromSeconds(1))
      _ <- network.publish(Outgoing.Send(
        Set(InetAddress.getByAddress(Array(192.toByte, 168.toByte, 4.toByte, 58.toByte))),
        SetPilotRequest(
          r = Some(255),
          g = Some(0),
          b = Some(255)
        )
      ))
      _ <- ZIO.sleep(Duration.fromSeconds(5))
    yield ()
  }

}
