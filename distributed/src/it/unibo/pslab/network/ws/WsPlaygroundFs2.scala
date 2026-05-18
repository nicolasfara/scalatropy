package it.unibo.pslab.network.ws

import cats.effect.{ IO, IOApp }
import fs2.*

object WsEchoServer extends IOApp.Simple:
  import org.http4s.*
  import org.http4s.dsl.io.*
  import org.http4s.server.websocket.WebSocketBuilder2
  import org.http4s.websocket.WebSocketFrame
  import org.http4s.ember.server.EmberServerBuilder
  import com.comcast.ip4s.*

  def routes(wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] =
    HttpRoutes.of[IO] { case GET -> Root / "ws" =>
      // Simply echo every text frame back with a prefix
      val echo: Pipe[IO, WebSocketFrame, WebSocketFrame] =
        _.collect { case WebSocketFrame.Text(text, _) =>
          WebSocketFrame.Text(s"echo: $text")
        }
      wsb.build(echo)
    }

  override def run: IO[Unit] =
    EmberServerBuilder
      .default[IO]
      .withHost(ipv4"127.0.0.1")
      .withPort(port"8080")
      .withHttpWebSocketApp(routes(_).orNotFound)
      .build
      .useForever

object WsEchoClient extends IOApp.Simple:
  import sttp.capabilities.fs2.Fs2Streams
  import sttp.client4.*
  import sttp.client4.httpclient.fs2.HttpClientFs2Backend
  import sttp.client4.ws.stream.*
  import sttp.ws.WebSocketFrame

  // The pipe receives incoming Data frames and produces outgoing frames.
  // Pattern: emit an initial message, then react to each reply.
  val pipe: Pipe[IO, WebSocketFrame.Data[?], WebSocketFrame] = incoming =>
    Stream.emit(WebSocketFrame.text("hello")) ++
      incoming.take(2).zipWithIndex.evalMap {
        case (WebSocketFrame.Text(msg, _, _), 0) =>
          IO.println(s"[client] received: $msg") *> IO.pure(WebSocketFrame.text("world"))

        case (WebSocketFrame.Text(msg, _, _), _) =>
          IO.println(s"[client] received: $msg") *> IO.pure(WebSocketFrame.close)

        case (other, _) =>
          IO.println(s"[client] unexpected frame: $other") *> IO.pure(WebSocketFrame.close)
      }

  override def run: IO[Unit] =
    HttpClientFs2Backend.resource[IO]().use { backend =>
      basicRequest
        .get(uri"ws://127.0.0.1:8080/ws")
        .response(asWebSocketStream(Fs2Streams[IO])(pipe))
        .send(backend)
        .void
    }
