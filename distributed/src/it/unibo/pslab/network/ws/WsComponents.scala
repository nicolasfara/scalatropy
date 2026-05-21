package it.unibo.pslab.network.ws

import it.unibo.pslab.network.PeerId

import cats.effect.{ Concurrent, Ref, Resource }
import cats.effect.std.{ Queue, Supervisor }
import cats.syntax.all.*
import fs2.{ Pipe, Stream }
import scodec.bits.ByteVector
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend
import upickle.default.Writer

trait WebSocketHandler[F[_]: Concurrent]:

  /** The map of currently alive peer connections, mapping peer IDs to their outgoing message queues. */
  protected val alivePeers: Ref[F, Map[String, Queue[F, Array[Byte]]]]

  /** Handles an incoming message payload from a connected peer. */
  def onMessage(payload: Array[Byte]): F[Unit]

  /**
   * Manages a WebSocket session with a connected peer, handling both incoming and outgoing messages and ensuring proper
   * connection lifecycle management.
   *
   * @param peerId
   *   the ID of the connected peer
   * @param q
   *   the queue for outgoing raw messages to this peer
   * @param incomingBytes
   *   the stream of incoming raw message payloads from this peer
   * @return
   *   a stream of outgoing raw message payloads to be processed and sent to the peer
   */
  def session(peerId: String, q: Queue[F, Array[Byte]], incomingBytes: Stream[F, Array[Byte]]): Stream[F, Array[Byte]] =
    val incomingHandler = incomingBytes.evalMap(onMessage)
    val outgoing = Stream.repeatEval(q.take)
    incomingHandler.drain
      .mergeHaltBoth(outgoing)
      .onFinalize(alivePeers.update(_ - peerId))
end WebSocketHandler

/**
 * Server-side WebSocket acceptor that listens and manages incoming connection requests from remote peers.
 */
trait WebSocketAcceptor[F[_]: Concurrent] extends WebSocketHandler[F]:
  import org.http4s.HttpRoutes
  import org.http4s.dsl.io.*
  import org.http4s.server.websocket.WebSocketBuilder2
  import org.http4s.websocket.WebSocketFrame

  /**
   * Sets up the HTTP routes to accept WebSocket connections and manage sessions with peers.
   * @param builder
   *   the WebSocketBuilder used to build WebSocket responses for incoming connection requests
   * @return
   *   the HTTP routes that handle WebSocket connection requests
   */
  def setup(builder: WebSocketBuilder2[F]): HttpRoutes[F] = HttpRoutes.of[F]:
    case req @ GET -> Root / "ws" / connectingPeerId =>
      for
        q <- Queue.unbounded[F, Array[Byte]]
        _ <- alivePeers.update(_ + (connectingPeerId -> q))
        response <- builder.build: incoming =>
          session(connectingPeerId, q, incoming).map(payload => WebSocketFrame.Binary(ByteVector(payload)))
      yield response

  private given Conversion[Stream[F, WebSocketFrame], Stream[F, Array[Byte]]] = _.collect:
    case WebSocketFrame.Binary(payload, true) => payload.toArray
    case WebSocketFrame.Text(payload, true)   => payload.getBytes
end WebSocketAcceptor

/**
 * Client-side WebSocket connector that manages outgoing connections to remote peers and emits messages to them.
 */
trait WebSocketConnector[F[_]: Concurrent](
    supervisor: Supervisor[F],
    backend: Resource[F, WebSocketStreamBackend[F, Fs2Streams[F]]],
) extends WebSocketHandler[F]:
  import com.comcast.ip4s.{ IpAddress, SocketAddress }
  import sttp.capabilities.fs2.Fs2Streams
  import sttp.client4.*
  import sttp.client4.ws.stream.*
  import sttp.ws.WebSocketFrame
  import sttp.model.Uri

  /**
   * Emits a message towards a remote peer.
   *
   * @param self
   *   the ID of the sending peer
   * @param remote
   *   the ID of the receiving peer
   * @param remoteAddr
   *   the network address of the receiving peer
   * @param data
   *   the message payload to send
   */
  def emit[Value: Writer](self: PeerId, remote: PeerId, remoteAddr: SocketAddress[IpAddress], data: Value): F[Unit] =
    for
      q <- getOrBootstrap(remote, self, remoteAddr)
      payload <- F.catchNonFatal(upickle.writeBinary(data))
      _ <- q.offer(payload)
    yield ()

  private def getOrBootstrap(self: PeerId, remote: PeerId, remoteAddr: SocketAddress[IpAddress]) =
    for
      q <- Queue.unbounded[F, Array[Byte]]
      updated <- alivePeers.modify: peers =>
        peers.get(remote.id) match
          case Some(existing) => (peers, (existing, false))
          case None           => (peers + (remote.id -> q), (q, true))
      res <- updated match
        case (q, true)  => supervisor.supervise(openConnection(remoteAddr, self.id, mkPipe(q, remote))).as(q)
        case (q, false) => q.pure
    yield res

  private def mkPipe(q: Queue[F, Array[Byte]], remote: PeerId): Pipe[F, WebSocketFrame.Data[?], WebSocketFrame] =
    incoming => session(remote.id, q, incoming).map(WebSocketFrame.binary)

  private def openConnection(
      remoteAddr: SocketAddress[IpAddress],
      selfId: String,
      pipe: Pipe[F, WebSocketFrame.Data[?], WebSocketFrame],
  ): F[Unit] =
    backend.use: backend =>
      basicRequest
        .get(Uri.unsafeParse("ws://" + remoteAddr.toString + "/ws/" + selfId))
        .response(asWebSocketStream(Fs2Streams[F])(pipe))
        .send(backend)
        .void

  private given Conversion[Stream[F, WebSocketFrame], Stream[F, Array[Byte]]] = _.collect:
    case WebSocketFrame.Binary(payload, true, _) => payload.toArray
    case WebSocketFrame.Text(payload, true, _)   => payload.getBytes
end WebSocketConnector
