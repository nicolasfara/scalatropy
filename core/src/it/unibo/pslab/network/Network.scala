package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList

/**
 * Marker trait to define communication protocols between peer types.
 * @example
 *   {{{trait IoT extends CommunicationProtocol}}}
 */
trait CommunicationProtocol

/**
 * The Network layer, implementing a [[CommunicationProtocol]], abstracting the underlying communication mechanism and
 * providing a uniform interface for sending and receiving messages between peers
 */
trait Network[F[_], Local <: Peer] extends CommunicationProtocol:
  type PeerId[P <: Peer]

  val local: PeerId[Local]

  def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: PeerId[To]): F[Unit]

  def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: PeerId[From]): F[V]

  /**
   * @return
   *   a [[NonEmptyList]] with all the reachable peers of type [[RP]].
   */
  def alivePeersOf[RP <: Peer: PeerTag]: F[NonEmptyList[PeerId[RP]]]

type NetworkManager[F[_], Local <: Peer, Id[_ <: Peer]] = Network[F, Local] & {
  type PeerId[P <: Peer] = Id[P]
}
