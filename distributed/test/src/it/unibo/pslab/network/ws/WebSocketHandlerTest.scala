package it.unibo.pslab.network.ws

import cats.effect.{ IO, Ref }
import cats.effect.std.Queue
import cats.effect.testing.scalatest.AsyncIOSpec
import fs2.Stream
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.implicits.catsSyntaxApplicativeId
import cats.Applicative
import WebSocketHandlerTest.*

class WebSocketHandlerTest extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  "WebSocketHandler" should "receive and process incoming messages" in {
    for
      receivedMessages <- Ref.of[IO, List[Array[Byte]]](List.empty)
      handler = new TestHandler(onIncomingMsg = payload => receivedMessages.update(_ :+ payload))
      q <- Queue.unbounded[IO, Array[Byte]]
      incomingPayload = "Hello".getBytes
      incoming = Stream.emit(incomingPayload)
      _ <- handler.session("peer-1", q, incoming).head.compile.drain
      messages <- receivedMessages.get
    yield
      messages should have length 1
      messages.head should equal(incomingPayload)
  }

  it should "queue outgoing messages" in {
    for
      handler = TestHandler(onIncomingMsg = doNothing)
      q <- Queue.unbounded[IO, Array[Byte]]
      outgoingPayload = "Outgoing".getBytes
      _ <- q.offer(outgoingPayload)
      received <- handler
        .session("peer-1", q, incomingBytes = Stream.never)
        .head
        .compile
        .toList
    yield
      received should have length 1
      received.head should equal(outgoingPayload)
  }

  it should "store connections and clean up on session end" in {
    for
      handler = TestHandler(onIncomingMsg = doNothing)
      q <- Queue.unbounded[IO, Array[Byte]]
      peerId = "peer-1"
      _ <- handler.alivePeers.update(_ + (peerId -> q))
      peersBefore <- handler.alivePeers.get
      _ <- handler.session(peerId, q, incomingBytes = Stream.empty).compile.drain
      peersAfter <- handler.alivePeers.get
    yield
      peersBefore.size should be(1)
      peersAfter.size should be(0)
  }
end WebSocketHandlerTest

object WebSocketHandlerTest:
  class TestHandler(onIncomingMsg: Array[Byte] => IO[Unit]) extends WebSocketHandler[IO]:
    val alivePeers = Ref.unsafe[IO, Map[String, Queue[IO, Array[Byte]]]](Map.empty)
    def onMessage(payload: Array[Byte]): IO[Unit] = onIncomingMsg(payload)

  def doNothing[F[_]: Applicative, T]: T => F[Unit] = _ => ().pure[F]
