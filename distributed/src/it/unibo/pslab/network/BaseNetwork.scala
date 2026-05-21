package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment
import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.Codable.{ decode, encode }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.effect.kernel.{ Concurrent, Deferred, Ref }
import cats.syntax.all.*
import upickle.default.ReadWriter

type PeerRef[P <: Peer] = PeerId

/**
 * A typed reference to a peer, combining a tag that identifies the peer's type at the type level with a string id that
 * uniquely identifies the instance.
 */
case class PeerId(tag: PeerTag[?], id: String) derives ReadWriter

object PeerId:
  def apply[LP <: Peer: PeerTag as peerTag](id: String) = new PeerRef(peerTag, id)

/**
 * A ScalaTropy application message, containing the sender's peer reference, the resource reference for synchronization,
 * and the encoded payload.
 */
case class ScalaTropyMessage(from: PeerRef[?], resource: Reference, payload: Array[Byte]) derives ReadWriter

object BaseNetwork:
  /**
   * A registry of deferred messages awaiting completion.
   *
   * Maps (peer address, resource reference) pairs to deferreds containing message payloads. This allows for
   * synchronization between senders and receivers, regardless of the order in which they arrive.
   */
  type IncomingMessages[F[_], Id] = Map[(Id, Environment.Reference), Deferred[F, Array[Byte]]]

  object IncomingMessages:
    def empty[F[_], Id]: IncomingMessages[F, Id] = Map.empty

/**
 * Base trait providing common functionality for Network implementations.
 */
trait BaseNetwork[F[_]: {Concurrent, NetworkMonitor as monitor}, LP <: Peer] extends Network[F, LP, PeerRef]:

  import BaseNetwork.IncomingMessages

  protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]]

  /**
   * Retrieves an existing deferred for a message from a specific peer and resource, or creates a new one if it doesn't
   * exist, allowing either send or receive to arrive first.
   * @return
   *   the Deferred that will be completed with the message payload
   */
  protected def takePeerMsgOrDefer[P <: Peer](key: (PeerRef[P], Environment.Reference)): F[Deferred[F, Array[Byte]]] =
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
  override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: PeerRef[From]): F[V] =
    for
      toWaitOn <- takePeerMsgOrDefer((from, resource))
      data <- toWaitOn.get.flatTap(monitor.onReceive).flatMap(decode)
    yield data

  override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: PeerRef[To]): F[Unit] =
    for
      encodedValue <- encode(value).flatTap(monitor.onSend)
      _ <- dispatch[To](to, ScalaTropyMessage(local, resource, encodedValue))
    yield ()

  /** Low-level dispatch method to send the given application message to the specified peer. */
  def dispatch[To <: Peer: PeerTag](to: PeerRef[To], message: ScalaTropyMessage): F[Unit]
