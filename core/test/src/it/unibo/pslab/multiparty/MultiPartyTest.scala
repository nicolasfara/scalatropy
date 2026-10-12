package it.unibo.pslab.multiparty

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.*
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.utils.{ ErrorOr, InMemoryTestNetwork, ScalaTropyTestEngine, StringPeerId }

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.syntax.all.*
import org.scalamock.stubs.Stubs
import org.scalatest.EitherValues.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class MultiPartyTest extends AnyFunSpec, should.Matchers, Stubs, ScalaTropyTestEngine:
  type Foo <: { type Tie <: via[* toMultiple Foo] & via[* toMultiple Baz] }
  type Bar <: Foo
  type Baz <: { type Tie <: via[* toSingle Foo] }

  describe("The on operator"):
    describe("when involves a peer class with subtypes"):
      it("should evaluate the expression even in the subtypes classes"):
        val result = testScalaTropyProgramInIsolation[ErrorOr, Bar]:
          var evaluations = 0
          for
            _ <- on[Foo]((evaluations += 1).pure[ErrorOr]) // this should be evaluated since `Bar` is a `Foo`
            _ <- on[Bar]((evaluations += 1).pure[ErrorOr]) // this should be evaluated since the peer is exactly `Bar`
            _ <- on[Baz]((evaluations += 1).pure[ErrorOr]) // this should not be evaluated since `Bar` is not a `Baz`
          yield evaluations
        result.isRight shouldBe true
        result.value shouldBe 2

  describe("Isotropic Communication"):
    describe("when involves a peer class with subtypes"):
      it("should allow sending messages from its subtypes classes"):
        def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
          res <- on[Foo](10.pure)
          placed <- isotropicComm[Foo, Baz](res)
        yield ()

        val network = stub[Network[[A] =>> Either[Throwable, A], Bar, StringPeerId]]
        (network.alivePeersOf(using _: PeerTag[Baz])).returnsWith(NonEmptyList.of("first").asRight)
        (network
          .send[Int, Baz](_: Int, _: Reference, _: StringPeerId[Baz])(using
            _: Encodable[[A] =>> Either[Throwable, A]][Int],
            _: PeerTag[Baz],
          ))
          .returnsWith(().asRight)
        ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Bar]:
          tiedTo[Foo] via network
          tiedTo[Baz] via network

        // Even if the `isotropicComm` has a sender Foo, Bar is a subtype of Foo, so it should be able to send the message as well.
        (network
          .send[Int, Baz](_: Int, _: Reference, _: StringPeerId[Baz])(using
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

      val network = InMemoryTestNetwork[Bar, Boolean]("selector", NonEmptyList.of("relay", "downstream"))
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Bar]:
        tiedTo[Foo] via network
        tiedTo[Baz] via network

      network.sent should contain theSameElementsInOrderAs List(true -> "relay", true -> "downstream")

    it("should forward a received selection to reachable peers except the sender"):
      def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
        decision <- on[Foo](true.pure)
        result <- select[Foo](decision)(_.pure)
      yield result shouldBe true

      val network =
        InMemoryTestNetwork[Baz, Boolean]("relay", NonEmptyList.of("selector", "downstream"), "selector" -> true :: Nil)
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Baz]:
        tiedTo[Foo] via network

      network.sent should contain theSameElementsInOrderAs List(true -> "downstream")

    it("should let a downstream peer take the branch from a relayed selection"):
      def testProgram[F[_]: MonadThrow](using MultiParty[F]) = for
        decision <- on[Foo](false.pure)
        result <- select[Foo](decision)(_.pure)
      yield result shouldBe false

      val network = InMemoryTestNetwork[Baz, Boolean]("downstream", NonEmptyList.of("relay"), "relay" -> false :: Nil)
      ScalaTropy(testProgram[[A] =>> Either[Throwable, A]]).projectedOn[Baz]:
        tiedTo[Foo] via network

      network.sent shouldBe empty
