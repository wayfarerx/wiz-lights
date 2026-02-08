package net.wayfarerx.wizlights
package backend
package network

import protocol.Response

import java.net.InetAddress

/**
 * An incoming network response.
 *
 * @param address  The address of the device that sent this response.
 * @param response The response payload that was received.
 */
case class Incoming(address: InetAddress, response: Response)
