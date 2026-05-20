package it.unibo.pslab.network.ws

import it.unibo.pslab.network.PeerRef
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.effect.{ Async, Ref }
import cats.effect.std.{ Queue, Supervisor }
import cats.implicits.{ toFlatMapOps, toFunctorOps }
import cats.syntax.all.*
import fs2.{ Pipe, Stream }
import scodec.bits.ByteVector
import upickle.default.Writer

trait WebSocketHandler[F[_]]:
  protected val alivePeers: Ref[F, Map[String, Queue[F, Array[Byte]]]]

  def onMessage(payload: Array[Byte]): F[Unit]

trait WebSocketAcceptor[F[_]: Async] extends WebSocketHandler[F]:
  import org.http4s.*
  import org.http4s.dsl.io.*
  import org.http4s.server.websocket.WebSocketBuilder2
  import org.http4s.websocket.WebSocketFrame

  def setup(wsb: WebSocketBuilder2[F]): HttpRoutes[F] = HttpRoutes.of[F]:
    case req @ GET -> Root / "ws" / peerId =>
      for
        q <- Queue.unbounded[F, Array[Byte]]
        _ <- alivePeers.update(_ + (peerId -> q))
        response <- wsb.build: incoming =>
          val incomingHandler = incoming
            .evalMap:
              case WebSocketFrame.Binary(payload, true) => onMessage(payload.toArray)
              case WebSocketFrame.Text(payload, true)   => onMessage(payload.getBytes)
              case _                                    => F.unit
          val outgoing = Stream.repeatEval(q.take).map(p => WebSocketFrame.Binary(ByteVector(p)))
          outgoing.concurrently(incomingHandler.drain).onFinalize(alivePeers.update(_ - peerId))
      yield response
end WebSocketAcceptor

trait WebSocketConnector[F[_]: Async] extends WebSocketHandler[F]:
  import sttp.capabilities.fs2.Fs2Streams
  import sttp.client4.*
  import sttp.client4.httpclient.fs2.HttpClientFs2Backend
  import sttp.client4.ws.stream.*
  import sttp.ws.WebSocketFrame
  import sttp.model.Uri

  protected val supervisor: Supervisor[F]

  def emit[To <: Peer: PeerTag, Value: Writer](to: PeerRef[To], selfId: String, url: String, data: Value): F[Unit] =
    for
      _ <- alivePeers.get >>= (peers => F.catchNonFatal(println(s"**** [WebSocketNetwork] ALIVE PEERS: ${peers}")))
      q <- alivePeers.get.flatMap(peers => peers.get(to.id).map(_.pure).getOrElse(bootstrap(to.id, selfId, url)))
      payload <- F.catchNonFatal(upickle.writeBinary(data))
      _ <- q.offer(payload)
    yield ()

  private def bootstrap(peerId: String, selfId: String, url: String): F[Queue[F, Array[Byte]]] =
    for
      q <- Queue.unbounded[F, Array[Byte]]
      _ <- alivePeers.update(_ + (peerId -> q))
      _ <- supervisor.supervise(
        openConnection(
          url,
          selfId,
          incoming =>
            val incomingHandler = incoming.evalMap:
              case WebSocketFrame.Text(payload, true, _)   => onMessage(payload.getBytes)
              case WebSocketFrame.Binary(payload, true, _) => onMessage(payload)
              case _                                       => F.unit
            val outgoing = Stream.repeatEval(q.take).map(WebSocketFrame.binary)
            // Let incoming drive: merge them so the stream only ends when
            // the incoming side signals close (not just when outgoing has nothing)
            incomingHandler.drain
              .mergeHaltBoth(outgoing) // halts when EITHER side finishes
              .onFinalize(alivePeers.update(_ - peerId) *> Async[F].delay(println(s"CLEANUP $peerId"))),
        ),
      )
    yield q

  private def openConnection(
      url: String,
      selfId: String,
      pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame],
  ): F[Unit] =
    println(s"==> [WebSocketNetwork] opening ws client to ws://${url}/ws/${selfId}")
    HttpClientFs2Backend // TODO: parametrize
      .resource[F]()
      .use: backend =>
        basicRequest
          .get(Uri.unsafeParse("ws://" + url + "/ws/" + selfId)) // TODO: handle invalid URL
          .response(asWebSocketStream(Fs2Streams[F])(pipe))
          .send(backend)
          .void
end WebSocketConnector
