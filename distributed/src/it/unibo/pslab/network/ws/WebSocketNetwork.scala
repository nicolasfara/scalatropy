package it.unibo.pslab.network.ws

import it.unibo.pslab.network.{
  BaseNetwork,
  Network,
  NetworkMonitor,
  NoSuchPeers,
  PeerId,
  PeerRef,
  ScalaTropyMessage,
  WS,
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

trait WebSocketNetwork[F[_], LP <: Peer] extends Network[F, LP, PeerRef], WS

object WebSocketNetwork:

  def make[F[_]: {Async, Concurrent, Fs2Network, NetworkMonitor}, LP <: Peer: PeerTag as localPeer](
      id: String,
      port: Int,
      knownPeers: Map[PeerId, SocketAddress[IpAddress]],
  ): Resource[F, WebSocketNetwork[F, LP]] =
    for
      alivePeers <- Resource.eval(Ref.of[F, Map[String, Queue[F, Array[Byte]]]](Map.empty))
      incomingMsgs <- Resource.eval(Ref.of[F, IncomingMessages[F, PeerRef[?]]](IncomingMessages.empty))
      supervisor <- Supervisor[F]
      impl = WebSocketNetworkImpl(id, port, knownPeers, alivePeers, incomingMsgs, supervisor)
      _ <- EmberServerBuilder
        .default[F]
        .withHost(host"localhost")
        .withPort(Port.fromInt(port).get) // TODO: handle invalid port
        .withHttpWebSocketApp(impl.setup(_).orNotFound)
        .build
    yield impl

  private class WebSocketNetworkImpl[
      F[_]: {Async, Concurrent, NetworkMonitor},
      LP <: Peer: PeerTag as localPeer,
  ](
      id: String,
      port: Int,
      connectedPeers: Map[PeerId, SocketAddress[IpAddress]],
      protected val alivePeers: Ref[F, Map[String, Queue[F, Array[Byte]]]],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]],
      protected val supervisor: Supervisor[F],
  ) extends WebSocketNetwork[F, LP],
        WebSocketAcceptor[F],
        WebSocketConnector[F],
        BaseNetwork[F, LP]:

    override val local: PeerRef[LP] = PeerId(localPeer, id)

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeer]: F[NonEmptyList[PeerId]] =
      val filtered = connectedPeers.keys.filter(_.tag <:< remotePeer)
      NonEmptyList.fromList(filtered.toList) match
        case Some(nel) => nel.pure
        case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeer))

    override def dispatch[To <: Peer: PeerTag](to: PeerRef[To], message: ScalaTropyMessage): F[Unit] =
      val toAddress = connectedPeers.getOrElse(to, throw new NoSuchPeers(to.tag))
      emit[To, ScalaTropyMessage](to, id, toAddress.toString, message)

    override def onMessage(payload: Array[Byte]): F[Unit] =
      for
        ScalaTropyMessage(address, resource, data) <- F.catchNonFatal(upickle.readBinary[ScalaTropyMessage](payload))
        d <- takePeerMsgOrDefer((address, resource))
        _ <- d.complete(data)
      yield ()
  end WebSocketNetworkImpl
end WebSocketNetwork
