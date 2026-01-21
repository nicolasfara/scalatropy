package it.unibo.pslab.multiparty

import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.multiparty.Environment.Resource
import MultiParty.on
import cats.Monad
import cats.Applicative

trait Label[+V]
trait Many[+V]
trait Anisotropic[+V]
trait Remote[-P <: Peer]

trait MultiParty[F[_]]:
  def anisotropicComm[From <: TiedWithMultiple[To], To <: Peer](using
    PeerTag[From],
    PeerTag[To]
  )[V](value: Anisotropic[V] on From): F[V on To]

  def anisotropicMessage[From <: TiedWithMultiple[To], To <: Peer](using
    PeerTag[From],
    PeerTag[To],
    Label[From]
  )[V](value: PartialFunction[Remote[To], V]): F[Anisotropic[V] on To]

  def comm[From <: TiedWithSingle[To], To <: Peer](using PeerTag[From], PeerTag[To])[V](value: V on From): F[V on To]
  def isotropicComm[From <: TiedWithMultiple[To], To <: Peer](using PeerTag[From], PeerTag[To])[V](value: V on From): F[Many[V] on To]
  def on[Local <: Peer](using PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local]
  def reachablePeers[RP <: Peer](using PeerTag[RP])[L <: Peer: Label]: F[Iterable[Remote[RP]]]
  def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V]

object MultiParty:
  opaque infix type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer](val res: Resource):
    case Local(override val res: Resource, value: V) extends Placement[V, P](res)
    case Remote(override val res: Resource) extends Placement[V, P](res)

  def apply[F[_]: MultiParty as mp]: MultiParty[F] = mp

  def on[Local <: Peer](using local: PeerTag[Local])[F[_]: Monad, V](body: Label[Local] ?=> F[V])(using lang: MultiParty[F]): F[V on Local] =
    lang.on[Local](body)

  def comm[From <: TiedWithSingle[To], To <: Peer](using PeerTag[From], PeerTag[To])[F[_]: Monad, V](value: V on From)(using lang: MultiParty[F]): F[V on To] =
    lang.comm[From, To](value)

  def take[Local <: Peer](using Label[Local])[F[_]: Monad, V](placed: V on Local)(using lang: MultiParty[F]): F[V] =
    lang.take[Local](placed)

  def project[F[_]: Applicative, P <: Peer: PeerTag](env: Environment[F], network: Network[F]): MultiParty[F] = new MultiParty[F]:
    def on[Local <: Peer](using local: PeerTag[Local])[V](body: Label[Local] ?=> F[V]): F[V on Local] =
      val p = summon[PeerTag[P]]
      given Label[Local] = new Label[Local] {}
      val resourceF = env.provide(local)
      if p == local then
        val valueF = body
        (resourceF, valueF).mapN(Placement.Local[V, Local](_, _))
      else
        resourceF.map(res => Placement.Remote[V, Local](res))

    def comm[From <: TiedWithSingle[To], To <: Peer](using from: PeerTag[From], to: PeerTag[To])[V](value: V on From): F[V on To] =
      val p = summon[PeerTag[P]]
      if p == from then
        val Placement.Local(res, v) = value.runtimeChecked
        network.send(v, res).map(_ => Placement.Remote[V, To](res))
      else if p == to then
        val Placement.Remote(res) = value.runtimeChecked
        network.receive[V](res).map(Placement.Local[V, To](res, _))
      else
        Applicative[F].pure(Placement.Remote[V, To](value.res))

    def isotropicComm[From <: TiedWithMultiple[To], To <: Peer](using PeerTag[From], PeerTag[To])[V](value: V on From): F[Many[V] on To] = ???

    def anisotropicComm[From <: TiedWithMultiple[To], To <: Peer](using PeerTag[From], PeerTag[To])[V](value: Anisotropic[V] on From): F[V on To] = ???

    def anisotropicMessage[From <: TiedWithMultiple[To], To <: Peer](using
      PeerTag[From],
      PeerTag[To],
      Label[From]
    )[V](value: PartialFunction[Remote[To], V]): F[Anisotropic[V] on To] = ???

    def reachablePeers[RP <: Peer](using PeerTag[RP])[L <: Peer: Label]: F[Iterable[Remote[RP]]] = ???

    def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V] =
      val Placement.Local(res, v) = placed.runtimeChecked
      Applicative[F].pure(v)
