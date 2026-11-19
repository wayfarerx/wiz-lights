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

import zio.{Task, ZIO}

/**
 * The configuration properties for the backend.
 *
 * @param outgoingWindow    The number of outgoing requests to keep in the network queue.
 * @param incomingWindow    The number of incoming responses to keep in the network hub.
 * @param commandPort       The network port to send commands on.
 * @param listenerPort      The network port to also listen on.
 * @param discoveryInterval The frequency of broadcast discovery attempts.
 * @param registrationTtl   The duration after which a bulb registration is refreshed.
 * @param purgeTimeout      The timeout for devices that do not respond.
 * @param retryBackoffs     The durations to wait between retrying outgoing messages.
 * @param knownDevices      The known device addresses for discovering lights across subnets.
 */
case class Configuration private(
  outgoingWindow: Int,
  incomingWindow: Int,
  commandPort: Int,
  listenerPort: Int,
  discoveryInterval: FiniteDuration,
  registrationTtl: FiniteDuration,
  purgeTimeout: FiniteDuration,
  retryBackoffs: List[FiniteDuration],
  knownDevices: Set[InetAddress]
)

/**
 * Factory for backend configurations.
 */
object Configuration:

  /** The default number of outgoing requests to keep in the network queue. */
  val DefaultOutgoingWindow: Int = 50

  /** The default number of incoming responses to keep in the network hub. */
  val DefaultIncomingWindow: Int = 500

  /** The default network port to send commands on. */
  val DefaultCommandPort: Int = 38899

  /** The default network port to listen on. */
  val DefaultListenerPort: Int = 38900

  /** The default frequency of broadcast discovery attempts. */
  val DefaultDiscoveryInterval: FiniteDuration = 25.seconds

  /** The default duration after which a bulb registration is refreshed. */
  val DefaultRegistrationTtl: FiniteDuration = 5.minutes

  /** The default timeout for devices that do not respond. */
  val DefaultPurgeTimeout: FiniteDuration = 3.minutes

  /** The default durations to wait between retrying outgoing messages. */
  val DefaultRetryBackoffs: List[FiniteDuration] = 200.milliseconds :: 350.milliseconds :: 500.milliseconds :: Nil

  /** The default known device addresses for discovering lights across subnets. */
  val DefaultKnownDevices: Set[InetAddress] = Set.empty

  /**
   * Creates a new backend configuration.
   *
   * @param outgoingWindow    The number of outgoing requests to keep in the network queue.
   * @param incomingWindow    The number of incoming responses to keep in the network hub.
   * @param commandPort       The network port to send commands on.
   * @param listenerPort      The network port to listen on.
   * @param discoveryInterval The frequency of broadcast discovery attempts.
   * @param purgeTimeout      The timeout for devices that do not respond.
   * @param retryBackoffs     The durations to wait between retrying outgoing messages.
   * @param registrationTtl   The duration after which a bulb registration is refreshed.
   * @param knownDevices      The known device addresses for discovering lights across subnets.
   * @return A new backend configuration.
   */
  def make(
    outgoingWindow: Int = DefaultOutgoingWindow,
    incomingWindow: Int = DefaultIncomingWindow,
    commandPort: Int = DefaultCommandPort,
    listenerPort: Int = DefaultListenerPort,
    discoveryInterval: FiniteDuration = DefaultDiscoveryInterval,
    registrationTtl: FiniteDuration = DefaultRegistrationTtl,
    purgeTimeout: FiniteDuration = DefaultPurgeTimeout,
    retryBackoffs: Iterable[FiniteDuration] = DefaultRetryBackoffs,
    knownDevices: Iterable[InetAddress] = DefaultKnownDevices
  ): Task[Configuration] =
    validNetworkPort(commandPort) validate
      validNetworkPort(listenerPort) validate
      validDiscoveryInterval(discoveryInterval) validate
      validRegistrationTtl(registrationTtl)  validate
      validPurgeTimeout(purgeTimeout) validate
      validRetryBackoffs(retryBackoffs)map { _ =>
      Configuration(
        outgoingWindow,
        incomingWindow,
        commandPort,
        listenerPort,
        discoveryInterval,
        registrationTtl,
        purgeTimeout,
        retryBackoffs.toList,
        knownDevices.toSet
      )
    }

  /**
   * Validates that the network port is a valid IP port.
   *
   * @param networkPort The network port to validate.
   */
  private def validNetworkPort(networkPort: Int): Task[Unit] =
    if networkPort >= 1 && networkPort <= 65535 then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid network port: $networkPort."))

  /**
   * Validates that the discovery interval is a positive duration.
   *
   * @param discoveryInterval The discovery interval to validate.
   */
  private def validDiscoveryInterval(discoveryInterval: FiniteDuration): Task[Unit] =
    if discoveryInterval > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid discovery interval: $discoveryInterval."))

  /**
   * Validates that the registration TTL is a positive duration.
   *
   * @param registrationTtl The registration TTL to validate.
   */
  private def validRegistrationTtl(registrationTtl: FiniteDuration): Task[Unit] =
    if registrationTtl > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid registration TTL: $registrationTtl."))

  /**
   * Validates that the purge timeout is a positive duration.
   *
   * @param purgeTimeout The purge timeout to validate.
   */
  private def validPurgeTimeout(purgeTimeout: FiniteDuration): Task[Unit] =
    if purgeTimeout > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid purge timeout: $purgeTimeout."))

  /**
   * Validates that the retry backoffs are all positive durations.
   *
   * @param retryBackoffs The retry backoffs to validate.
   */
  private def validRetryBackoffs(retryBackoffs: Iterable[FiniteDuration]): Task[Unit] =
    retryBackoffs.foldLeft(ZIO attempt ())((u, r) => u validate validRetryBackoff(r) map (_ => ()))

  /**
   * Validates that a retry backoff is a positive duration.
   *
   * @param retryBackoff The retry backoff to validate.
   */
  private def validRetryBackoff(retryBackoff: FiniteDuration): Task[Unit] =
    if retryBackoff > Duration.Zero then ZIO.unit else
      ZIO.fail(IllegalArgumentException(s"Invalid retry backoff: $retryBackoff."))
