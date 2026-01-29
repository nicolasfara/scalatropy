package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }
import cats.data.NonEmptyList

trait Network[F[_], LP <: Peer]:
  type Address[P <: Peer]

  val localAddress: Address[LP]

  def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit]

  def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V]

  /**
   * @return
   *   a [[NonEmptyList]] with all the reachable peers of type [[RP]].
   */
  def alivePeersOf[RP <: Peer: PeerTag]: F[NonEmptyList[Address[RP]]]
