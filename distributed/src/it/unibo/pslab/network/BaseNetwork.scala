package it.unibo.pslab.network

import it.unibo.pslab.multiparty.Environment
import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.Codable.{ decode, encode }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
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
  final case class FirstArrivalWaiter[F[_], Id](
      from: Set[Id],
      deferred: Deferred[F, (Id, Array[Byte])],
  )

  /**
   * A registry of deferred messages awaiting completion.
   *
   * Maps (peer address, resource reference) pairs to deferreds containing message payloads. This allows for
   * synchronization between senders and receivers, regardless of the order in which they arrive.
   */
  type IncomingMessages[F[_], Id] = Map[(Id, Environment.Reference), Deferred[F, Array[Byte]]]
  type FirstArrivalMessages[F[_], Id] = Map[Environment.Reference, List[FirstArrivalWaiter[F, Id]]]

  object IncomingMessages:
    def empty[F[_], Id]: IncomingMessages[F, Id] = Map.empty

  object FirstArrivalMessages:
    def empty[F[_], Id]: FirstArrivalMessages[F, Id] = Map.empty

/**
 * Base trait providing common functionality for Network implementations.
 */
trait BaseNetwork[F[_]: {Concurrent, NetworkMonitor as monitor}, LP <: Peer] extends Network[F, LP, PeerRef]:

  import BaseNetwork.{ FirstArrivalWaiter, IncomingMessages }

  protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]]
  protected val firstArrivalMsgs: Ref[F, BaseNetwork.FirstArrivalMessages[F, PeerRef[Peer]]]

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

  override def receiveFromAny[V: Decodable[F]](
      resource: Reference,
      from: NonEmptyList[PeerRef[Peer]],
  ): F[(PeerRef[Peer], V)] =
    val allowed = from.toList.toSet
    def takeAlreadyArrived: F[Option[(PeerRef[?], Array[Byte])]] =
      incomingMsgs.get.flatMap: messages =>
        messages.toList
          .collectFirstSomeM:
            case ((sender, `resource`), message) if allowed(sender) =>
              message.tryGet.map(_.map(sender -> _))
            case _ => none[(PeerRef[?], Array[Byte])].pure
          .flatTap:
            case Some((sender, _)) => incomingMsgs.update(_ - ((sender, resource)))
            case None              => Concurrent[F].unit

    for
      deferred <- Deferred[F, (PeerRef[Peer], Array[Byte])]
      arrived <- takeAlreadyArrived
      encoded <- arrived match
        case Some(value) => value.pure
        case None        =>
          for
            toWaitOn <- firstArrivalMsgs.modify: messages =>
              val waiter = FirstArrivalWaiter(allowed, deferred)
              messages.get(resource) match
                case Some(found) => (messages.updated(resource, found :+ waiter), deferred)
                case None        => (messages.updated(resource, List(waiter)), deferred)
            value <- toWaitOn.get
          yield value
      (sender, data) = encoded
      decoded <- monitor.onReceive(data) >> decode[F, V](data)
    yield sender -> decoded

  override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: PeerRef[To]): F[Unit] =
    sendToAny(value, resource, to)

  override def sendToAny[V: Encodable[F]](value: V, resource: Reference, to: PeerRef[Peer]): F[Unit] =
    for
      encodedValue <- encode(value).flatTap(monitor.onSend)
      _ <- dispatchAny(to, ScalaTropyMessage(local, resource, encodedValue))
    yield ()

  protected def deliver(from: PeerRef[?], resource: Reference, payload: Array[Byte]): F[Unit] =
    for
      firstArrival <- firstArrivalMsgs.modify: messages =>
        val waiters = messages.getOrElse(resource, Nil)
        val (matching, remaining) = waiters.partition(_.from(from))
        val updated =
          if remaining.isEmpty then messages - resource
          else messages.updated(resource, remaining)
        (updated, matching.headOption.map(_.deferred))
      _ <- firstArrival match
        case Some(waiter) => waiter.complete(from -> payload).void
        case None         =>
          for
            exact <- takePeerMsgOrDefer((from, resource))
            _ <- exact.complete(payload)
          yield ()
    yield ()

  /** Low-level dispatch method to send the given application message to the specified peer. */
  def dispatch[To <: Peer: PeerTag](to: PeerRef[To], message: ScalaTropyMessage): F[Unit]

  def dispatchAny(to: PeerRef[?], message: ScalaTropyMessage): F[Unit]
