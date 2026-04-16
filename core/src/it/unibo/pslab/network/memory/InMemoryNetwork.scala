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
import it.unibo.pslab.network.BaseNetwork.IncomingMessages
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.kernel.{ Concurrent, Ref, Resource }
import cats.effect.std.Console
import cats.syntax.all.*

object InMemoryNetwork:

  case class Address(tag: PeerTag[?], id: String)

  object Address:
    def apply[LP <: Peer: PeerTag as peerTag](id: String) = new Address(peerTag, id)

  case class NetworkNotRegistered(address: Address) extends NetworkError(s"$address not registered in dispatcher")

  trait MessagesDispatcher[F[_]]:
    def register(address: Address, network: NetworkHandle[F]): F[Unit]

    def unregister(address: Address): F[Unit]

    def route[To <: Peer: PeerTag](payload: Array[Byte], resource: Reference, from: Address, to: Address): F[Unit]

  trait NetworkHandle[F[_]]:
    def deliver(payload: Array[Byte], resource: Reference, from: Address): F[Unit]

  def messagesDispatcher[F[_]: Concurrent: Console]: F[MessagesDispatcher[F]] = Ref
    .of[F, Map[Address, NetworkHandle[F]]](Map.empty)
    .map: registry =>
      new MessagesDispatcher[F]:
        override def register(address: Address, network: NetworkHandle[F]) = registry.update(_ + (address -> network))
        override def unregister(address: Address) = registry.update(_ - address)
        override def route[To <: Peer: PeerTag](payload: Array[Byte], resource: Reference, from: Address, to: Address) =
          registry.get.flatMap: networks =>
            networks.get(to) match
              case Some(handle) => handle.deliver(payload, resource, from)
              case None         => Concurrent[F].raiseError(NetworkNotRegistered(to))

  def apply[F[_]: {Concurrent, Console, NetworkMonitor}, LP <: Peer: PeerTag as localPeerTag](
      localId: String,
      knownPeers: NonEmptyList[Address],
      messagesDispatcher: MessagesDispatcher[F],
  ): Resource[F, Network[F, LP]] =
    Resource.make(
      for
        incomingMsgs <- Ref.of(IncomingMessages.empty[F, Address])
        localAddress = Address(localPeerTag, localId)
        network = new InMemoryNetworkImpl(localAddress, knownPeers, messagesDispatcher, incomingMsgs)
        _ <- messagesDispatcher.register(localAddress, network.asHandle)
      yield network,
    )(network => messagesDispatcher.unregister(network.localAddress))

  private class InMemoryNetworkImpl[F[_]: {Concurrent, NetworkMonitor}, LP <: Peer: PeerTag](
      override val localAddress: Address,
      knownPeers: NonEmptyList[Address],
      messagesDispatcher: MessagesDispatcher[F],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, Address]],
  ) extends BaseNetwork[F, LP, Address]
      with Memory:
    override type Address[P <: Peer] = InMemoryNetwork.Address

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[NonEmptyList[Address[RP]]] =
      val filtered = knownPeers.toList.filter(_.tag == remotePeerTag)
      NonEmptyList.fromList(filtered) match
        case Some(nel) => nel.pure[F]
        case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeerTag))

    override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit] =
      for
        encodedValue <- encodeAndTrack(value)
        _ <- messagesDispatcher.route(encodedValue, resource, localAddress, to)
      yield ()

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V] =
      receiveImpl(resource, from)

    def asHandle: NetworkHandle[F] = new NetworkHandle[F]:
      override def deliver(payload: Array[Byte], resource: Reference, from: InMemoryNetwork.Address): F[Unit] =
        for
          existing <- takePeerMsgOrDefer((from, resource))
          _ <- existing.complete(payload)
        yield ()
