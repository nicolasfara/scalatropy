package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

trait Network[F[_], LP <: Peer]:
  type Address[P <: Peer]
  type Format

  val localAddress: Address[LP]

  def send[V: EncodableTo[F, Format], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit]
  def receive[V: DecodableFrom[F, Format], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V]
  // def receiveAll[V, From <: Peer: PeerTag, To <: Peer: PeerTag](resource: Reference, from: Address[From]): F[Map[Address[To], V]]
  def alivePeersOf[RP <: Peer: PeerTag]: F[Iterable[Address[RP]]]
