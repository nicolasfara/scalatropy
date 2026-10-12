package it.unibo.pslab.libraries

import it.unibo.pslab.libraries.Syntax.*
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.*
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.utils.{ ErrorOr, ScalaTropyTestEngine }

import cats.syntax.all.*
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class SyntaxTest extends AnyFunSpec, should.Matchers, ScalaTropyTestEngine:

  type Node <: { type Tie <: via[* toMultiple Node] }

  describe("take2"):
    it("should correctly take two elements placed on a peer"):
      testScalaTropyProgramInIsolation[ErrorOr, Node]:
        for
          value1 <- on[Node](10.pure[ErrorOr])
          value2 <- on[Node](20.pure[ErrorOr])
          _ <- on[Node]:
            take2(value1, value2).map: (v1, v2) =>
              v1 shouldBe 10
              v2 shouldBe 20
        yield ()
