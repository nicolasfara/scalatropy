package it.unibo.pslab.network.ws

import it.unibo.pslab.network.{
  BaseNetwork,
  Network,
  NetworkMonitor,
  NoSuchPeers,
  PeerId,
  PeerRef,
  ScalaTropyMessage,
  WebSocket,
}
import it.unibo.pslab.network.BaseNetwork.IncomingMessages
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.{ Async, Concurrent, Ref }
import cats.effect.kernel.Resource
import cats.effect.std.{ Queue, Supervisor }
import cats.syntax.all.*
import com.comcast.ip4s.{ host, IpAddress, Port, SocketAddress }
import fs2.io.net.Network as Fs2Network
import org.http4s.ember.server.EmberServerBuilder
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend
import sttp.client4.httpclient.fs2.HttpClientFs2Backend

trait WebSocketNetwork[F[_], LP <: Peer] extends Network[F, LP, PeerRef], WebSocket

object WebSocketNetwork:

  def make[F[_]: {Async, Concurrent, Fs2Network, NetworkMonitor}, LP <: Peer: PeerTag as localPeer](
      id: String,
      port: Port,
      knownPeers: Map[PeerId, SocketAddress[IpAddress]],
  ): Resource[F, WebSocketNetwork[F, LP]] =
    for
      alivePeers <- Resource.eval(Ref.of[F, Map[String, Queue[F, Array[Byte]]]](Map.empty))
      incomingMsgs <- Resource.eval(Ref.of[F, IncomingMessages[F, PeerRef[?]]](IncomingMessages.empty))
      supervisor <- Supervisor[F]
      backend = HttpClientFs2Backend.resource[F]()
      impl = WebSocketNetworkImpl(id, port, knownPeers, supervisor, backend, alivePeers, incomingMsgs)
      _ <- EmberServerBuilder
        .default[F]
        .withHost(host"localhost")
        .withPort(port)
        .withHttpWebSocketApp(impl.setup(_).orNotFound)
        .build
    yield impl

  private class WebSocketNetworkImpl[
      F[_]: {Async, Concurrent, NetworkMonitor},
      LP <: Peer: PeerTag as localPeer,
  ](
      id: String,
      port: Port,
      connectedPeers: Map[PeerId, SocketAddress[IpAddress]],
      supervisor: Supervisor[F],
      backend: Resource[F, WebSocketStreamBackend[F, Fs2Streams[F]]],
      protected val alivePeers: Ref[F, Map[String, Queue[F, Array[Byte]]]],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]],
  ) extends WebSocketNetwork[F, LP],
        WebSocketConnector[F](supervisor, backend),
        WebSocketAcceptor[F],
        BaseNetwork[F, LP]:

    override val local: PeerRef[LP] = PeerId(localPeer, id)

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeer]: F[NonEmptyList[PeerId]] =
      val filtered = connectedPeers.keys.filter(_.tag <:< remotePeer)
      NonEmptyList.fromList(filtered.toList) match
        case Some(nel) => nel.pure
        case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeer))

    override def dispatch[To <: Peer: PeerTag](to: PeerRef[To], message: ScalaTropyMessage): F[Unit] =
      for
        toAddress <- F.fromOption(connectedPeers.get(to), new NoSuchPeers(to.tag))
        _ <- emit(local, to, toAddress, message)
      yield ()

    override def onMessage(payload: Array[Byte]): F[Unit] =
      for
        ScalaTropyMessage(address, resource, data) <- F.catchNonFatal(upickle.readBinary[ScalaTropyMessage](payload))
        d <- takePeerMsgOrDefer((address, resource))
        _ <- d.complete(data)
      yield ()
  end WebSocketNetworkImpl
end WebSocketNetwork
