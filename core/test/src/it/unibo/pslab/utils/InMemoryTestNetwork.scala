package it.unibo.pslab.utils

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{ Decodable, Encodable, Network }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.syntax.all.*

type ErrorOr[A] = Either[Throwable, A]

/**
 * A minimal, synchronous, in-memory [[Network]] test double, useful to unit test multi-party operators without wiring
 * an actual network.
 * @param received
 *   the sender and value to be returned by `receiveFromAny`, if any
 */
final class InMemoryTestNetwork[Local <: Peer, Message](
    override val local: StringPeerId[Local],
    alive: NonEmptyList[StringPeerId[Peer]],
    received: List[(StringPeerId[Peer], Message)] = Nil,
) extends Network[ErrorOr, Local, StringPeerId]:

  private var sentMessages: List[(Message, StringPeerId[Peer])] = Nil

  /** The messages sent so far, in order, paired with their recipient. */
  def sent: List[(Message, StringPeerId[Peer])] = sentMessages

  override def send[V: Encodable[ErrorOr], To <: Peer: PeerTag](
      value: V,
      resource: Reference,
      to: StringPeerId[To],
  ): ErrorOr[Unit] =
    sendToAny(value, resource, to)

  override def sendToAny[V: Encodable[ErrorOr]](value: V, resource: Reference, to: StringPeerId[Peer]): ErrorOr[Unit] =
    sentMessages = sentMessages :+ (value.asInstanceOf[Message] -> to)
    ().asRight

  override def receive[V: Decodable[ErrorOr], From <: Peer: PeerTag](
      resource: Reference,
      from: StringPeerId[From],
  ): ErrorOr[V] =
    received
      .filter(_._1 == from)
      .map(_._2.asInstanceOf[V])
      .headOption
      .toRight(RuntimeException(s"No message received for $from"))

  override def receiveFromAny[V: Decodable[ErrorOr]](
      resource: Reference,
      from: NonEmptyList[StringPeerId[Peer]],
  ): ErrorOr[(StringPeerId[Peer], V)] =
    received
      .filter { case (sender, _) => from.exists(_ == sender) }
      .map((sender, value) => sender -> value.asInstanceOf[V])
      .headOption
      .toRight(RuntimeException("No message received"))

  override def alivePeersOf[RP <: Peer: PeerTag]: ErrorOr[NonEmptyList[StringPeerId[RP]]] =
    alive.asInstanceOf[NonEmptyList[StringPeerId[RP]]].asRight

  override def alivePeers[UP <: Peer]: ErrorOr[NonEmptyList[StringPeerId[UP]]] =
    alive.asInstanceOf[NonEmptyList[StringPeerId[UP]]].asRight
