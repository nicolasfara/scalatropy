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

type PeerRef[P <: Peer] = String

class MultiPartyTest extends AnyFunSpec, should.Matchers, Stubs:
  type Foo <: { type Tie <: via[AnyProtocol toMultiple Foo] }
  type Bar <: Foo
  type Baz <: { type Tie <: via[AnyProtocol toMultiple Foo] }

  describe("The on operator"):
    describe("when involves a peer class with subtypes"):
      it("should evaluate the expression even in the subtypes classes"):
        def testProgram[F[_]: Monad](using MultiParty[F]) =
          var counter = 0
          for
            _ <- on[Foo] { (counter += 1).pure }
            _ <- on[Bar] { (counter += 1).pure }
            _ <- on[Baz] { (counter += 1).pure }
          yield
            counter shouldBe 2
            ()
        val network = stub[Network[Id, Bar, PeerRef]]
        ScalaTropy(testProgram[Id]).projectedOn[Bar]:
          tiedTo[Foo] via network
