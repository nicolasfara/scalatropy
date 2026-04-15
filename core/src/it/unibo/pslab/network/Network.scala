package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList

/**
 * Marker trait to define communication protocols between peer types.
 * @example
 *   {{{trait MQTT extends CommunicationProtocol}}}
 */
trait CommunicationProtocol

/**
 * The Network layer, implementing a [[CommunicationProtocol]], abstracting the underlying communication mechanism and
 * providing a uniform interface for sending and receiving messages between peers
 */
trait Network[F[_], LP <: Peer] extends CommunicationProtocol:
  type Address[P <: Peer]

  val localAddress: Address[LP]

  def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit]

  def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V]

  /**
   * @return
   *   a [[NonEmptyList]] with all the reachable peers of type [[RP]].
   */
  def alivePeersOf[RP <: Peer: PeerTag]: F[NonEmptyList[Address[RP]]]
