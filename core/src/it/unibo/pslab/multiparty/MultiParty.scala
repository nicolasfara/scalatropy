package it.unibo.pslab.multiparty

import scala.compiletime.Erased

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.multiparty.MultiParty.on
import it.unibo.pslab.network.{ Codable, Network }
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, Peer, PeerTag, TiedWithMultiple, TiedWithSingle }

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.syntax.all.*

trait Label[+V] extends Erased

trait MultiParty[F[_]: Monad]:
  type Remote[P <: Peer]
  type Anisotropic[P <: Peer, V]

  def reachablePeers[RP <: Peer](using PeerTag[RP])[L <: Peer: Label]: F[NonEmptyList[Remote[RP]]]

  def on[Local <: Peer](using PeerTag[Local])[V](body: (Label[Local]) ?=> F[V]): F[V on Local]

  def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V]

  def takeAll[Local <: Peer, RP <: Peer](using
      Label[Local],
  )[V](placed: Anisotropic[RP, V] on Local): F[Map[Remote[RP], V]]

  def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[V: Codable[F]](value: V on From): F[V on To]

  def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[V: Codable[F]](value: V on From): F[V on To]

  def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      Label[From],
  )[V](value: Map[Remote[To], V], default: V): F[Anisotropic[To, V]]

  def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[V: Codable[F]](value: Anisotropic[To, V] on From): F[V on To]

  def coAnisotropicComm[From <: TiedWithSingle[To], To <: TiedWithMultiple[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[V: Codable[F]](value: V on From): F[Anisotropic[From, V] on To]

  def fresh: MultiParty[F]

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer](val res: Reference):
    case Local(override val res: Reference, value: V) extends Placement[V, P](res)
    case Remote(override val res: Reference) extends Placement[V, P](res)

  def apply[F[_]: MultiParty as mp]: MultiParty[F] = mp

  def reachablePeers[RP <: Peer](using
      PeerTag[RP],
  )[F[_], L <: Peer: Label](using lang: MultiParty[F]): F[NonEmptyList[lang.Remote[RP]]] =
    lang.reachablePeers[RP]

  def on[Local <: Peer](using
      PeerTag[Local],
  )[F[_], V](body: Label[Local] ?=> F[V])(using lang: MultiParty[F]): F[V on Local] =
    lang.on[Local](body)

  def take[Local <: Peer](using Label[Local])[F[_], V](placed: V on Local)(using lang: MultiParty[F]): F[V] =
    lang.take[Local](placed)

  def takeAll[Local <: Peer, RP <: Peer](using
      Label[Local],
  )[F[_], V](using lang: MultiParty[F])(placed: lang.Anisotropic[RP, V] on Local): F[Map[lang.Remote[RP], V]] =
    lang.takeAll(placed)

  def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[F[_]](using lang: MultiParty[F])[V: Codable[F]](value: V on From): F[V on To] =
    lang.comm[From, To](value)

  def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[F[_], V: Codable[F]](using lang: MultiParty[F])(value: V on From): F[V on To] =
    lang.isotropicComm[From, To](value)

  def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      Label[From],
  )[F[_], V](using lang: MultiParty[F])(value: Map[lang.Remote[To], V], default: V): F[lang.Anisotropic[To, V]] =
    lang.anisotropicMessage[From, To](value, default)

  def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[F[_], V: Codable[F]](using lang: MultiParty[F])(value: lang.Anisotropic[To, V] on From): F[V on To] =
    lang.anisotropicComm[From, To](value)

  def coAnisotropicComm[From <: TiedWithSingle[To], To <: TiedWithMultiple[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
  )[F[_], V: Codable[F]](using lang: MultiParty[F])(value: V on From): F[lang.Anisotropic[From, V] on To] =
    lang.coAnisotropicComm[From, To](value)

  // format: off
  def make[F[_]: MonadThrow, P <: Peer: PeerTag, PeerId[_ <: Peer]](
      env: Environment[F],
      networks: Map[PeerTag[?], Network[F, P, PeerId]],
  ): MultiParty[F] =
    new MultiParty[F]:
      type Remote[P <: Peer] = PeerId[P]
      opaque type Anisotropic[P <: Peer, V] = Map[Remote[P], V]

      private val runtimePeer = summon[PeerTag[P]]
      
      def fresh: MultiParty[F] = make(Environment.make[F], networks)

      def reachablePeers[RP <: Peer](using remote: PeerTag[RP])[L <: Peer: Label]: F[NonEmptyList[Remote[RP]]] =
        networkOf(remote) >>= (_.alivePeersOf[RP])

      def on[Local <: Peer](using local: PeerTag[Local])[V](body: (Label[Local]) ?=> F[V]): F[V on Local] =
        given Label[Local] = new Label[Local] {}
        val resourceF = env.provide(local)
        if runtimePeer <:< local then (resourceF, body).mapN(Placement.Local[V, Local].apply)
        else resourceF.map(Placement.Remote[V, Local].apply)

      def take[Local <: Peer](using Label[Local])[V](placed: V on Local): F[V] =
        val Placement.Local(res, v) = placed.runtimeChecked
        v.pure

      def takeAll[Local <: Peer, RP <: Peer](using
          Label[Local],
      )[V](placed: Anisotropic[RP, V] on Local): F[Map[Remote[RP], V]] =
        val Placement.Local(res, vMap) = placed.runtimeChecked
        vMap.pure

      def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
          PeerTag[From],
          PeerTag[To],
          CommunicationProtocolCompliance[From, To],
      )[V: Codable[F]](value: V on From): F[V on To] =
        foldRuntimePeer(ifLocal = network =>
          val Placement.Local(ref, v) = value.runtimeChecked
          for
            receiver <- network.alivePeersOf[To].map(_.head)
            _ <- network.send(v, ref, receiver)
          yield Placement.Remote(ref)
        )(ifRemote = network =>
          val Placement.Remote(res) = value.runtimeChecked
          for
            sender <- network.alivePeersOf[From].map(_.head)
            value <- network.receive(res, sender)
          yield Placement.Local(res, value),
        )(default = Placement.Remote(value.res).pure)

      def isotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
          PeerTag[From],
          PeerTag[To],
          CommunicationProtocolCompliance[From, To],
      )[V: Codable[F]](value: V on From): F[V on To] =
        foldRuntimePeer(ifLocal = network =>
          val Placement.Local(ref, v) = value.runtimeChecked
          for
            receivers <- network.alivePeersOf[To]
            _ <- receivers.toList.traverse(network.send(v, ref, _))
          yield Placement.Remote(ref)
        )(ifRemote = network =>
          val Placement.Remote(res) = value.runtimeChecked
          for
            sender <- network.alivePeersOf[From].map(_.head)
            value <- network.receive(res, sender)
          yield Placement.Local(res, value),
        )(default = Placement.Remote(value.res).pure)

      def anisotropicMessage[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
          from: PeerTag[From],
          to: PeerTag[To],
          l: Label[From],
      )[V](value: Map[Remote[To], V], default: V): F[Anisotropic[To, V]] =
        val runtimePeer = summon[PeerTag[P]]
        if runtimePeer == from then value.withDefault(_ => default).pure else Map.empty.pure

      def anisotropicComm[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
          PeerTag[From],
          PeerTag[To],
          CommunicationProtocolCompliance[From, To],
      )[V: Codable[F]](value: Anisotropic[To, V] on From): F[V on To] =
        foldRuntimePeer(ifLocal = network =>
          val Placement.Local(ref, vMap) = value.runtimeChecked
          for
            receivers <- network.alivePeersOf[To]
            _ <- receivers.toList.traverse: address =>
              val v = vMap(address)
              network.send(v, ref, address)
          yield Placement.Remote(ref)
        )(ifRemote = network =>
          val Placement.Remote(res) = value.runtimeChecked
          for
            sender <- network.alivePeersOf[From].map(_.head)
            value <- network.receive(res, sender)
          yield Placement.Local(res, value),
        )(default = Placement.Remote(value.res).pure)

      def coAnisotropicComm[From <: TiedWithSingle[To], To <: TiedWithMultiple[From]](using
          PeerTag[From],
          PeerTag[To],
          CommunicationProtocolCompliance[From, To],
      )[V: Codable[F]](value: V on From): F[Anisotropic[From, V] on To] =
        foldRuntimePeer(ifLocal = network =>
          val Placement.Local(ref, v) = value.runtimeChecked
          for
            receiver <- network.alivePeersOf[To].map(_.head)
            _ <- network.send(v, ref, receiver)
          yield Placement.Remote(ref)
        )(ifRemote = network =>
          val Placement.Remote(res) = value.runtimeChecked
          for
            senders <- network.alivePeersOf[From]
            values <- senders.toList.traverse(s => network.receive(res, s).map(s -> _))
          yield Placement.Local(res, values.toMap),
        )(default = Placement.Remote(value.res).pure)

      private type Handler[Result] = Network[F, P, PeerId] => F[Result]

      def foldRuntimePeer[From <: Peer: PeerTag as sourcePeer, To <: Peer: PeerTag as targetPeer](using
          matchingProtocol: CommunicationProtocolCompliance[From, To],
      )[Result](ifLocal: Handler[Result])(ifRemote: Handler[Result])(default: => F[Result]): F[Result] =
        if runtimePeer <:< sourcePeer then networkOf(targetPeer) >>= ifLocal
        else if runtimePeer <:< targetPeer then networkOf(sourcePeer) >>= ifRemote
        else default

      def networkOf(remotePeer: PeerTag[?]): F[Network[F, P, PeerId]] =
        networks
          .collectFirst { case (peer, network) if remotePeer <:< peer => network }
          .liftTo[F](RuntimeException(s"No network found for $remotePeer. This should not happen, report this!"))
