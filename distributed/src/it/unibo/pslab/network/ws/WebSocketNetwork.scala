package it.unibo.pslab.network.ws

import it.unibo.pslab.network.{
  ScalaTropyMessage,
  BaseNetwork,
  Network,
  NetworkMonitor,
  NoSuchPeers,
  PeerId,
  PeerRef,
  WS,
}
import it.unibo.pslab.network.BaseNetwork.IncomingMessages
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.{ Async, Concurrent, Ref }
import cats.effect.std.Queue
import cats.syntax.all.*
import cats.effect.syntax.all.*
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s.Port
import fs2.io.net.Network as Fs2Network
import com.comcast.ip4s.IpAddress
import com.comcast.ip4s.SocketAddress
import com.comcast.ip4s.host

trait WebSocketNetwork[F[_], LP <: Peer] extends Network[F, LP, PeerRef], WS

object WebSocketNetwork:

  def make[F[_]: {Async, Concurrent, Fs2Network, NetworkMonitor}, LP <: Peer: PeerTag as localPeer](
      id: String,
      port: Int,
      knownPeers: Map[PeerId, SocketAddress[IpAddress]],
  ): F[WebSocketNetwork[F, LP]] =
    for
      alivePeers <- Ref.of[F, Map[String, Queue[F, Array[Byte]]]](Map.empty)
      incomingMsgs <- Ref.of[F, IncomingMessages[F, PeerRef[?]]](IncomingMessages.empty)
      impl = WebSocketNetworkImpl(id, port, knownPeers, alivePeers, incomingMsgs)
      _ <- EmberServerBuilder
        .default[F]
        .withHost(host"localhost") // TODO: handle invalid address
        .withPort(Port.fromInt(port).get) // TODO: handle invalid port
        .withHttpWebSocketApp(impl.setup(_).orNotFound)
        .build
        .useForever
        .start
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
      println(s"TO Address: ${toAddress}")
      emit[To, ScalaTropyMessage](to, id, toAddress.toString, message)

    override def onMessage(payload: Array[Byte]): F[Unit] =
      for
        _ <- F.catchNonFatal(println(s"[WebSocketNetwork] received message with payload size ${payload.length}"))
        ScalaTropyMessage(address, resource, data) <- F.catchNonFatal(upickle.readBinary[ScalaTropyMessage](payload))
        _ <- F.catchNonFatal(println(s"[WebSocketNetwork] decoded message from ${address} for resource ${resource}"))
        d <- takePeerMsgOrDefer((address, resource))
        _ <- d.complete(data)
      yield ()
  end WebSocketNetworkImpl
end WebSocketNetwork
