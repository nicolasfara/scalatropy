package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.Codable.{ decode, encode }
import it.unibo.pslab.peers.Peers.Peer

import cats.effect.kernel.{ Concurrent, Deferred, Ref }
import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.PeerTag

import upickle.ReadWriter

object BaseNetwork:
  /**
   * Type alias for a registry of deferred messages awaiting completion.
   *
   * Maps (peer address, resource reference) pairs to deferreds containing message payloads. This allows for
   * synchronization between senders and receivers, regardless of the order in which they arrive.
   */
  type IncomingMessages[F[_], PeerId] = Map[(PeerId, Reference), Deferred[F, Array[Byte]]]

  object IncomingMessages:
    def empty[F[_], PeerId]: IncomingMessages[F, PeerId] = Map.empty

  case class PeerId(tag: PeerTag[?], id: String) derives ReadWriter

  object PeerId:
    def apply[LP <: Peer: PeerTag as peerTag](id: String) = new PeerId(peerTag, id)

/**
 * Base trait providing common functionality for Network implementations.
 */
trait BaseNetwork[F[_]: {Concurrent, NetworkMonitor}, LP <: Peer] extends Network[F, LP]:

  import BaseNetwork.{ IncomingMessages, PeerId as Id }

  protected val incomingMsgs: Ref[F, IncomingMessages[F, Id]]

  override type PeerId[P <: Peer] = Id

  /**
   * Retrieves an existing deferred for a message from a specific peer and resource, or creates a new one if it doesn't
   * exist, allowing either send or receive to arrive first.
   * @return
   *   the Deferred that will be completed with the message payload
   */
  protected def takePeerMsgOrDefer(key: (Id, Reference)): F[Deferred[F, Array[Byte]]] =
    for
      d <- Deferred[F, Array[Byte]]
      res <- incomingMsgs.modify: m =>
        m.get(key) match
          case Some(found) => (m - key, found)
          case None        => (m.updated(key, d), d)
    yield res

  /**
   * Receives a value from a specific peer and resource. This method waits for the message to arrive, monitors the
   * receive, and decodes the payload.
   * @return
   *   the decoded value of type V
   */
  def receiveImpl[V: Decodable[F]](resource: Reference, from: Id): F[V] =
    for
      toWaitOn <- takePeerMsgOrDefer((from, resource))
      data <- toWaitOn.get.flatTap(summon[NetworkMonitor[F]].onReceive).flatMap(decode)
    yield data

  /**
   * Encodes a value and tracks it with the network monitor.
   * @return
   *   the encoded value as an array of bytes
   */
  protected def encodeAndTrack[V: Encodable[F]](value: V): F[Array[Byte]] =
    encode(value).flatTap(summon[NetworkMonitor[F]].onSend)
