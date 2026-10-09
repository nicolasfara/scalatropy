package it.unibo.pslab

import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.*
import it.unibo.pslab.peers.Peers.*

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import org.scalamock.stubs.Stubs
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

type PeerRef[P <: Peer] = String

object UpickleCodable:
  export upickle.default as upickle
  export upickle.*

  given [F[_]: MonadThrow, T: ReadWriter]: CodableF[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = F.catchNonFatal(writeBinary(value))
    override inline def decode(data: Array[Byte]): F[T] = F.catchNonFatal(readBinary[T](data))

class MultiPartyTest extends AnyFunSpec, should.Matchers, Stubs:
  type Foo <: { type Tie <: via[AnyProtocol toMultiple Foo] & via[AnyProtocol toMultiple Baz] }
  type Bar <: Foo
  type Baz <: { type Tie <: via[AnyProtocol toSingle Foo] }

  final class SelectNetwork[Local <: Peer](
      override val local: PeerRef[Local],
      alive: NonEmptyList[PeerRef[Peer]],
      received: Option[(PeerRef[Peer], Boolean)] = None,
  ) extends Network[[A] =>> Either[Throwable, A], Local, PeerRef]:
    var sent: List[(Boolean, PeerRef[Peer])] = Nil

    override def send[V: Encodable[[A] =>> Either[Throwable, A]], To <: Peer: PeerTag](
        value: V,
        resource: Reference,
        to: PeerRef[To],
    ): Either[Throwable, Unit] =
      sendToAny(value, resource, to)

    override def sendToAny[V: Encodable[[A] =>> Either[Throwable, A]]](
        value: V,
        resource: Reference,
        to: PeerRef[Peer],
    ): Either[Throwable, Unit] =
      sent = sent :+ (value.asInstanceOf[Boolean] -> to)
      ().asRight

    override def receive[V: Decodable[[A] =>> Either[Throwable, A]], From <: Peer: PeerTag](
        resource: Reference,
        from: PeerRef[From],
    ): Either[Throwable, V] =
      RuntimeException("receive should not be used by select tests").asLeft

    override def receiveFromAny[V: Decodable[[A] =>> Either[Throwable, A]]](
        resource: Reference,
        from: NonEmptyList[PeerRef[Peer]],
    ): Either[Throwable, (PeerRef[Peer], V)] =
      received
        .map((sender, value) => sender -> value.asInstanceOf[V])
        .toRight(RuntimeException("No selection received"))

    override def alivePeersOf[RP <: Peer: PeerTag]: Either[Throwable, NonEmptyList[PeerRef[RP]]] =
      alive.asInstanceOf[NonEmptyList[PeerRef[RP]]].asRight

    override def alivePeers[UP <: Peer]: Either[Throwable, NonEmptyList[PeerRef[UP]]] =
      alive.asInstanceOf[NonEmptyList[PeerRef[UP]]].asRight

  describe("The on operator"):
    describe("when involves a peer class with subtypes"):
      it("should evaluate the expression even in the subtypes classes"):
        def testProgram[F[_]: Monad](using MultiParty[F]) =
          var counter = 0
          for
            _ <- on[Foo]((counter += 1).pure)
            _ <- on[Bar]((counter += 1).pure)
            _ <- on[Baz]((counter += 1).pure)
          yield
            counter shouldBe 2
            ()
        val network = stub[Network[IO, Bar, PeerRef]]
        ScalaTropy(testProgram[IO]).projectedOn[Bar]:
          tiedTo[Foo] via network
          tiedTo[Baz] via network

  describe("Isotropic Communication"):
    describe("when involves a peer class with subtypes"):
      it("should allow sending messages from its subtypes classes"):
        def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
          res <- on[Foo](10.pure)
          placed <- isotropicComm[Foo, Baz](res)
        yield ()

        val network = stub[Network[[A] =>> Either[Throwable, A], Bar, PeerRef]]
        (network.alivePeersOf(using _: PeerTag[Baz])).returnsWith(NonEmptyList.of("first").asRight)
        (network
          .send[Int, Baz](_: Int, _: Reference, _: PeerRef[Baz])(using
            _: Encodable[[A] =>> Either[Throwable, A]][Int],
            _: PeerTag[Baz],
          ))
          .returnsWith(().asRight)
        ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Bar]:
          tiedTo[Foo] via network
          tiedTo[Baz] via network

        // Even if the `isotropicComm` has a sender Foo, Bar is a subtype of Foo, so it should be able to send the message as well.
        (network
          .send[Int, Baz](_: Int, _: Reference, _: PeerRef[Baz])(using
            _: Encodable[[A] =>> Either[Throwable, A]][Int],
            _: PeerTag[Baz],
          ))
          .times shouldBe 1

  describe("The select operator"):
    it("should spread the selected value from the peer that owns the decision"):
      def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
        decision <- on[Foo](true.pure)
        result <- select[Foo](decision)(_.pure)
      yield result shouldBe true

      val network = SelectNetwork[Bar]("selector", NonEmptyList.of("relay", "downstream"))
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Bar]:
        tiedTo[Foo] via network
        tiedTo[Baz] via network

      network.sent should contain theSameElementsInOrderAs List(true -> "relay", true -> "downstream")

    it("should forward a received selection to reachable peers except the sender"):
      def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
        decision <- on[Foo](true.pure)
        result <- select[Foo](decision)(_.pure)
      yield result shouldBe true

      val network = SelectNetwork[Baz]("relay", NonEmptyList.of("selector", "downstream"), Some("selector" -> true))
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Baz]:
        tiedTo[Foo] via network

      network.sent should contain theSameElementsInOrderAs List(true -> "downstream")

    it("should let a downstream peer take the branch from a relayed selection"):
      def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
        decision <- on[Foo](false.pure)
        result <- select[Foo](decision)(_.pure)
      yield result shouldBe false

      val network = SelectNetwork[Baz]("downstream", NonEmptyList.of("relay"), Some("relay" -> false))
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Baz]:
        tiedTo[Foo] via network

      network.sent shouldBe empty
