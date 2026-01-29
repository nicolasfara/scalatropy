package it.unibo.pslab.multiparty

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{ Codable, Network }
import it.unibo.pslab.peers.Peers.*

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.all.*

import MultiParty.on

trait Label[+V]

trait MultiParty[F[_]]:
  type Remote[P <: Peer]
  type Anisotropic[V]

  def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
  )[V: Codable[F]](value: Anisotropic[V] on From): F[V on To]

  def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      Label[From],
  )[V](value: Map[Remote[To], V], default: V): F[Anisotropic[V]]

  def comm[From <: TiedWithSingle[To], To <: Peer](using
      PeerTag[From],
      PeerTag[To],
  )[V: Codable[F]](value: V on From): F[V on To]

  def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
  )[V: Codable[F]](value: V on From): F[V on To]

  def on[Local <: Peer](using PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local]

  def reachablePeers[RP <: Peer](using PeerTag[RP])[L <: Peer: Label]: F[NonEmptyList[Remote[RP]]]

  def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V]

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer](val res: Reference):
    case Local(override val res: Reference, value: V) extends Placement[V, P](res)
    case Remote(override val res: Reference) extends Placement[V, P](res)

  def apply[F[_]: MultiParty as mp]: MultiParty[F] = mp

  def on[Local <: Peer](using
      local: PeerTag[Local],
  )[F[_]: Monad, V](body: Label[Local] ?=> F[V])(using lang: MultiParty[F]): F[V on Local] =
    lang.on[Local](body)

  def comm[From <: TiedWithSingle[To], To <: Peer](using
      PeerTag[From],
      PeerTag[To],
  )[F[_]: Monad](using lang: MultiParty[F])[V: Codable[F]](value: V on From): F[V on To] =
    lang.comm[From, To](value)

  def take[Local <: Peer](using Label[Local])[F[_]: Monad, V](placed: V on Local)(using lang: MultiParty[F]): F[V] =
    lang.take[Local](placed)

  def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
  )[F[_]: Monad](using lang: MultiParty[F])[V: Codable[F]](value: V on From): F[V on To] =
    lang.isotropicComm[From, To](value)

  def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
  )[F[_]: Monad](using
      lang: MultiParty[F],
  )[V: Codable[F]](value: lang.Anisotropic[V] on From): F[V on To] =
    lang.anisotropicComm[From, To](value)

  def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      Label[From],
  )[F[_]: Monad, V](using
      lang: MultiParty[F],
  )(value: Map[lang.Remote[To], V], default: V): F[lang.Anisotropic[V]] =
    lang.anisotropicMessage[From, To](value, default)

  def reachablePeers[RP <: Peer](using
      PeerTag[RP],
  )[F[_]: Monad, L <: Peer: Label](using
      lang: MultiParty[F],
  ): F[NonEmptyList[lang.Remote[RP]]] =
    lang.reachablePeers[RP]

  def make[F[_]: Monad, P <: Peer: PeerTag](
      env: Environment[F],
      network: Network[F, P],
  ): MultiParty[F] = new MultiParty[F]:
    type Remote[P <: Peer] = network.Address[P]
    opaque type Anisotropic[V] = Map[Any, V]

    // override given [P <: Peer] => Order[Remote[P]] = network.given_Order_Address

    def on[Local <: Peer](using local: PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local] =
      val runtimerPeer = summon[PeerTag[P]]
      given Label[Local] = new Label[Local] {}
      val resourceF = env.provide(local)
      if runtimerPeer == local then (resourceF, body).mapN(Placement.Local[V, Local](_, _))
      else resourceF.map(Placement.Remote[V, Local](_))

    def comm[From <: TiedWithSingle[To], To <: Peer](using
        from: PeerTag[From],
        to: PeerTag[To],
    )[V: Codable[F]](value: V on From): F[V on To] =
      val runtimePeer = summon[PeerTag[P]]
      if runtimePeer == from then
        val Placement.Local(res, v) = value.runtimeChecked
        for
          receiver <- network.alivePeersOf[To].map(_.head)
          _ <- network.send(v, res, receiver)
        yield Placement.Remote[V, To](res)
      else if runtimePeer == to then
        val Placement.Remote(res) = value.runtimeChecked
        for
          sender <- network.alivePeersOf[From].map(_.head)
          value <- network.receive(res, sender)
        yield Placement.Local[V, To](res, value)
      else Placement.Remote[V, To](value.res).pure[F]

    def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
        from: PeerTag[From],
        to: PeerTag[To],
    )[V: Codable[F]](value: V on From): F[V on To] =
      val runtimePeer = summon[PeerTag[P]]
      if runtimePeer == from then
        val Placement.Local(res, v) = value.runtimeChecked
        for
          receivers <- network.alivePeersOf[To]
          _ <- receivers.toList.traverse(network.send(v, res, _))
        yield Placement.Remote[V, To](res)
      else if runtimePeer == to then
        val Placement.Remote(res) = value.runtimeChecked
        for
          senders <- network.alivePeersOf[From]
          value <- senders.toList.traverse(network.receive[V, From](res, _)).map(_.head)
        yield Placement.Local[V, To](res, value)
      else Placement.Remote[V, To](value.res).pure[F]

    def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
        from: PeerTag[From],
        to: PeerTag[To],
    )[V: Codable[F]](value: Anisotropic[V] on From): F[V on To] =
      val runtimePeer = summon[PeerTag[P]]
      if runtimePeer == from then
        val Placement.Local(res, vMap) = value.runtimeChecked
        for
          receivers <- network.alivePeersOf[To]
          _ <- receivers.toList.traverse { address =>
            val v = vMap(address)
            network.send(v, res, address)
          }
        yield Placement.Remote[V, To](res)
      else if runtimePeer == to then
        val Placement.Remote(res) = value.runtimeChecked
        for
          senders <- network.alivePeersOf[From]
          value <- senders.toList.traverse(network.receive[V, From](res, _)).map(_.head)
        yield Placement.Local[V, To](res, value)
      else Placement.Remote[V, To](value.res).pure[F]

    def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
        from: PeerTag[From],
        to: PeerTag[To],
        l: Label[From],
    )[V](value: Map[Remote[To], V], default: V): F[Anisotropic[V]] =
      val runtimePeer = summon[PeerTag[P]]
      if runtimePeer == from then value.map(_.asInstanceOf[Any] -> _).withDefault(_ => default).pure
      else Map.empty.pure

    def reachablePeers[RP <: Peer](using PeerTag[RP])[L <: Peer: Label]: F[NonEmptyList[Remote[RP]]] =
      network.alivePeersOf[RP]

    def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V] =
      val Placement.Local(res, v) = placed.runtimeChecked
      v.pure[F]
