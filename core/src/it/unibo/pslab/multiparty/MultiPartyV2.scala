package it.unibo.pslab.multiparty

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.multiparty.MultiPartyV2.on
import it.unibo.pslab.network.{ Codable, Network }
import it.unibo.pslab.peers.PeersV2.{ CommunicationProtocolEvidence, Peer, PeerTag, TiedWithSingle }

import cats.Monad
import cats.syntax.all.*

trait MultiPartyV2[F[_]]:

  def on[Local <: Peer](using PeerTag[Local])[V](body: (Label[Local]) ?=> F[V]): F[V on Local]

  def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V]

  def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolEvidence[From, To],
  )[V: Codable[F]](value: V on From): F[V on To]

object MultiPartyV2:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  private[multiparty] enum Placement[+V, -P <: Peer](val res: Reference):
    case Local(override val res: Reference, value: V) extends Placement[V, P](res)
    case Remote(override val res: Reference) extends Placement[V, P](res)

  def apply[F[_]: MultiParty as mp]: MultiParty[F] = mp

  def on[Local <: Peer](using
      PeerTag[Local],
  )[F[_]: Monad, V](body: Label[Local] ?=> F[V])(using lang: MultiPartyV2[F]): F[V on Local] =
    lang.on[Local](body)

  def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolEvidence[From, To],
  )[F[_]: Monad](using lang: MultiPartyV2[F])[V: Codable[F]](value: V on From): F[V on To] =
    lang.comm[From, To](value)

  def take[Local <: Peer](using Label[Local])[F[_]: Monad, V](placed: V on Local)(using lang: MultiPartyV2[F]): F[V] =
    lang.take[Local](placed)

  def make[F[_]: Monad, P <: Peer: PeerTag](
      env: Environment[F],
      networks: Set[Network[F, P]],
  ): MultiPartyV2[F] = new MultiPartyV2[F]:

    override def on[Local <: Peer](using local: PeerTag[Local])[V](body: (Label[Local]) ?=> F[V]): F[V on Local] =
      val runtimerPeer = summon[PeerTag[P]]
      given Label[Local] = new Label[Local] {}
      val resourceF = env.provide(local)
      if runtimerPeer == local then (resourceF, body).mapN(Placement.Local[V, Local](_, _))
      else resourceF.map(Placement.Remote[V, Local](_))

    override def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V] =
      val Placement.Local(res, v) = placed.runtimeChecked
      v.pure

    override def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
        from: PeerTag[From],
        to: PeerTag[To],
        matchingProtocol: CommunicationProtocolEvidence[From, To],
    )[V: Codable[F]](value: V on From): F[V on To] =
      val runtimePeer = summon[PeerTag[P]]
      val network = networks.find(matchingProtocol).get
      if runtimePeer == from then
        val Placement.Local(res, v) = value.runtimeChecked
        for
          receiver <- network.alivePeersOf[To].map(_.head)
          _ <- network.send(v, res, receiver)
        yield Placement.Remote(res)
      else if runtimePeer == to then
        val Placement.Remote(res) = value.runtimeChecked
        for
          sender <- network.alivePeersOf[From].map(_.head)
          value <- network.receive(res, sender)
        yield Placement.Local(res, value)
      else Placement.Remote(value.res).pure
