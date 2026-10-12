package it.unibo.pslab.libraries

import it.unibo.pslab.libraries.SyncLibrary.requestReply
import it.unibo.pslab.multiparty.MultiParty.{ on, take }
import it.unibo.pslab.network.`*`
import it.unibo.pslab.peers.Peers.{ syntesizePeerTag, toSingle, via }
import it.unibo.pslab.utils.{ ErrorOr, InMemoryTestNetwork, ScalaTropyTestEngine }

import cats.data.NonEmptyList
import cats.syntax.all.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import it.unibo.pslab.peers.Peers.toMultiple
import it.unibo.pslab.libraries.SyncLibrary.requestReplies

class SyncLibraryTest extends AnyFunSpec, should.Matchers, ScalaTropyTestEngine:

  describe("requestReply"):
    describe("in a 1-1 peer tie relationship"):

      type Requester <: { type Tie <: via[* toSingle Responder] }
      type Responder <: { type Tie <: via[* toSingle Requester] }

      it("should correctly perform a request-reply operation"):
        val request = 10
        val handle: Int => Int = _ * 10
        val expected = handle(request)

        val network = InMemoryTestNetwork[Requester, Int](
          local = "requester",
          alive = NonEmptyList.of("responder"),
          received = "responder" -> expected :: Nil,
        )
        val result = testScalaTropyProgram[ErrorOr, Requester](network):
          for
            req <- on[Requester](request.pure[ErrorOr])
            res <- requestReply[Requester, Responder](req)(x => handle(x).pure[ErrorOr])
            _ <- on[Requester]:
              take(res).map(_ shouldBe expected)
          yield ()
        result.isRight shouldBe true

  describe("requestReplies"):
    describe("in a 1-to-many peer tie relationship"):

      type Requester <: { type Tie <: via[* toMultiple Responder] }
      type Responder <: { type Tie <: via[* toSingle Requester] }

      it("should correctly perform a request-replies operation"):
        val request = 10
        val handle: Int => Int = _ * 10
        val expected = handle(request)

        val network = InMemoryTestNetwork[Requester, Int](
          local = "requester",
          alive = NonEmptyList.of("responder1", "responder2"),
          received = "responder1" -> expected :: "responder2" -> expected :: Nil,
        )
        val result = testScalaTropyProgram[ErrorOr, Requester](network):
          for
            req <- on[Requester](request.pure[ErrorOr])
            res <- requestReplies[Requester, Responder](req)(x => handle(x).pure[ErrorOr])
            _ <- on[Requester]:
              take(res).map(_ shouldBe Map("responder1" -> expected, "responder2" -> expected))
          yield ()
        result.isRight shouldBe true
