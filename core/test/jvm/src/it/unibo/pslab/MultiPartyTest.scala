package it.unibo.pslab

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.network.*
import cats.Id
import cats.Monad
import cats.syntax.all.*
import org.scalamock.stubs.Stubs
import it.unibo.pslab.UpickleCodable.given
import cats.MonadThrow
import cats.data.NonEmptyList
import it.unibo.pslab.multiparty.Environment.Reference

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
        val network = stub[Network[Id, Bar, PeerRef]]
        ScalaTropy(testProgram[Id]).projectedOn[Bar]:
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
