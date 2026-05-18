package it.unibo.pslab.network.ws

import java.net.InetAddress

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{
  BaseNetwork,
  Decodable,
  Encodable,
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
import cats.effect.kernel.{ Concurrent, Ref }
import cats.effect.std.Queue
import cats.syntax.all.*
import fs2.{ Pipe, Stream }
import sttp.ws.WebSocketFrame

trait WebSocketNetwork[F[_], LP <: Peer] extends Network[F, LP, PeerRef], WS

object WebSocketNetwork:

  private class WebSocketNetworkImpl[
      F[_]: {Concurrent, NetworkMonitor},
      LP <: Peer: PeerTag as localPeer,
  ](
      alivePeers: Ref[F, Map[PeerRef[?], Queue[F, WebSocketFrame]]],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerRef[?]]],
  ) extends WebSocketNetwork[F, LP]
      with BaseNetwork[F, LP]:

    override val local: PeerRef[LP] = PeerId(localPeer, InetAddress.getLocalHost.getHostAddress())

    private def connectToPeer(peer: PeerRef[?], url: String): F[Unit] =
      for
        q <- Queue.unbounded[F, WebSocketFrame]
        _ <- alivePeers.update(_ + (peer -> q))
        pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame] = incoming =>
          val incomingHandler = incoming.evalMap:
            case WebSocketFrame.Text(payload, true, _)   => onApplicationMsg(payload.getBytes)
            case WebSocketFrame.Binary(payload, true, _) => onApplicationMsg(payload)
            case _ => F.unit // TODO: gracefully handle this case that shouldn't happen in our protocol
          Stream.repeatEval(q.take).concurrently(incomingHandler)
        _ <- openConnection(url, pipe)
      yield ()

    private def openConnection(url: String, pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame]): F[Unit] =
      ???

    private def onApplicationMsg(data: Array[Byte]): F[Unit] =
      for
        (address, resource, payload) <- F.catchNonFatal(upickle.readBinary[(PeerRef[?], Reference, Array[Byte])](data))
        d <- takePeerMsgOrDefer((address, resource))
        _ <- d.complete(payload)
      yield ()

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeer]: F[NonEmptyList[PeerId]] =
      // TODO: this is equal to the alivePeersOf in MQTT... refactor to avoid code duplication
      alivePeers.get.flatMap: peers =>
        val filtered = peers.keys.filter(_.tag <:< remotePeer)
        NonEmptyList.fromList(filtered.toList) match
          case Some(nel) => nel.pure
          case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeer))

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: PeerRef[From]): F[V] =
      receiveImpl(resource, from)

    override def send[V: Encodable[F], To <: Peer: PeerTag as targetPeerTag](
        value: V,
        resource: Reference,
        to: PeerRef[To],
    ): F[Unit] =
      for
        encodedValue <- encodeAndTrack(value)
        payload = upickle.writeBinary((local, resource, encodedValue))
        _ <- alivePeers.get.flatMap: peers =>
          peers.get(to) match
            case Some(queue) => queue.offer(WebSocketFrame.binary(payload))
            case None        => Concurrent[F].raiseError(NoSuchPeers(targetPeerTag))
      yield ()
