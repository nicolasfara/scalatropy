package it.unibo.pslab.network.memory

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{
  BaseNetwork,
  Decodable,
  Encodable,
  Memory,
  Network,
  NetworkError,
  NetworkMonitor,
  NoSuchPeers,
}
import it.unibo.pslab.network.BaseNetwork.{ IncomingMessages, PeerId }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.kernel.{ Concurrent, Ref, Resource }
import cats.effect.std.Console
import cats.syntax.all.*

trait InMemoryNetwork[F[_], LP <: Peer] extends Network[F, LP] with Memory

object InMemoryNetwork:
  export BaseNetwork.PeerId
  export BaseNetwork.PeerId.*

  case class NetworkNotRegistered(peerId: PeerId) extends NetworkError(s"$peerId not registered in dispatcher")

  trait MessagesDispatcher[F[_]]:
    def register(address: PeerId, network: NetworkHandle[F]): F[Unit]

    def unregister(address: PeerId): F[Unit]

    def route[To <: Peer: PeerTag](payload: Array[Byte], resource: Reference, from: PeerId, to: PeerId): F[Unit]

  trait NetworkHandle[F[_]]:
    def deliver(payload: Array[Byte], resource: Reference, from: PeerId): F[Unit]

  def messagesDispatcher[F[_]: Concurrent: Console]: F[MessagesDispatcher[F]] = Ref
    .of[F, Map[PeerId, NetworkHandle[F]]](Map.empty)
    .map: registry =>
      new MessagesDispatcher[F]:
        override def register(address: PeerId, network: NetworkHandle[F]) = registry.update(_ + (address -> network))
        override def unregister(address: PeerId) = registry.update(_ - address)
        override def route[To <: Peer: PeerTag](payload: Array[Byte], resource: Reference, from: PeerId, to: PeerId) =
          registry.get.flatMap: networks =>
            networks.get(to) match
              case Some(handle) => handle.deliver(payload, resource, from)
              case None         => Concurrent[F].raiseError(NetworkNotRegistered(to))

  def apply[F[_]: {Concurrent, Console, NetworkMonitor}, LP <: Peer: PeerTag as localPeerTag](
      localId: String,
      knownPeers: NonEmptyList[PeerId],
      messagesDispatcher: MessagesDispatcher[F],
  ): Resource[F, InMemoryNetwork[F, LP]] =
    Resource.make
      (
        for
          incomingMsgs <- Ref.of(IncomingMessages.empty[F, PeerId])
          localAddress = PeerId(localPeerTag, localId)
          network = new InMemoryNetworkImpl(localAddress, knownPeers, messagesDispatcher, incomingMsgs)
          _ <- messagesDispatcher.register(localAddress, network.asHandle)
        yield network,
      )
      (network => messagesDispatcher.unregister(network.local))

  private class InMemoryNetworkImpl[F[_]: {Concurrent, NetworkMonitor}, LP <: Peer: PeerTag](
      override val local: PeerId,
      knownPeers: NonEmptyList[PeerId],
      messagesDispatcher: MessagesDispatcher[F],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerId]],
  ) extends InMemoryNetwork[F, LP]
      with BaseNetwork[F, LP]:

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[NonEmptyList[BaseNetwork.PeerId]] =
      val filtered = knownPeers.toList.filter(_.tag == remotePeerTag)
      NonEmptyList.fromList(filtered) match
        case Some(nel) => nel.pure[F]
        case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeerTag))

    override def send[V: Encodable[F], To <: Peer: PeerTag](
        value: V,
        resource: Reference,
        to: BaseNetwork.PeerId,
    ): F[Unit] =
      for
        encodedValue <- encodeAndTrack(value)
        _ <- messagesDispatcher.route(encodedValue, resource, local, to)
      yield ()

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: BaseNetwork.PeerId): F[V] =
      receiveImpl(resource, from)

    def asHandle: NetworkHandle[F] = new NetworkHandle[F]:
      override def deliver(payload: Array[Byte], resource: Reference, from: BaseNetwork.PeerId): F[Unit] =
        for
          existing <- takePeerMsgOrDefer((from, resource))
          _ <- existing.complete(payload)
        yield ()
