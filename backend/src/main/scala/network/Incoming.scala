package net.wayfarerx.wizlights
package backend
package network

import java.net.InetAddress

import protocol.Response

/**
 * An incoming network response.
 *
 * @param address  The address of the device that sent this response.
 * @param response The response payload that was received.
 */
case class Incoming(address: InetAddress, response: Response)
