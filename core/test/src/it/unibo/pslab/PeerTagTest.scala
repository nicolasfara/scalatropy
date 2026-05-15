package it.unibo.pslab

import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.peers.Peers.{ syntesizePeerTag, toSingle, via }

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class PeerTagTest extends AnyFunSpec, should.Matchers:
  type Foo <: { type Tie <: via[AnyProtocol toSingle Bar] }
  type Bar <: { type Tie <: via[AnyProtocol toSingle Foo] }
  type Baz <: { type Tie <: via[AnyProtocol toSingle Baz] }
  type Qux <: Baz
  type Qez <: Qux & Baz

  describe("A PeerTag"):
    it("should models the runtime classifier of a peer"):
      val tag1 = syntesizePeerTag[Foo]
      val tag2 = syntesizePeerTag[Foo]
      tag1 <:< tag1 shouldBe true

  describe("Two unrelated PeerTags"):
    it("should not be in a subtype relation"):
      val tag1 = syntesizePeerTag[Foo]
      val tag2 = syntesizePeerTag[Bar]
      tag1 <:< tag2 shouldBe false
      tag2 <:< tag1 shouldBe false

  describe("Two PeerTags"):
    it("should be in a subtype relation if one is a supertype of the other"):
      val tag1 = syntesizePeerTag[Baz]
      val tag2 = syntesizePeerTag[Qux]
      tag2 <:< tag1 shouldBe true
      tag1 <:< tag2 shouldBe false

    it("should be in a subtyping relationship transitively"):
      val tag1 = syntesizePeerTag[Baz]
      val tag2 = syntesizePeerTag[Qux]
      val tag3 = syntesizePeerTag[Qez]

      tag2 <:< tag1 shouldBe true
      tag3 <:< tag2 shouldBe true
      tag3 <:< tag1 shouldBe true
      tag1 <:< tag3 shouldBe false
