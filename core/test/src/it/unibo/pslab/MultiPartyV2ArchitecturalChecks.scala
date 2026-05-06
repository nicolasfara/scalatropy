package it.unibo.pslab

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should
import scala.compiletime.testing.{ Error, typeCheckErrors }

class MultiPartyV2ArchitecturalChecks extends AnyFunSpec with should.Matchers:

  inline val commonImports = """
    import _root_.it.unibo.pslab.peers.PeersV2.*
    import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    import _root_.it.unibo.pslab.multiparty.MultiPartyV2.*
    import _root_.it.unibo.pslab.network.{ CommunicationProtocol, Codable }

    import cats.Monad
    import cats.syntax.all.*

    trait MQTT extends CommunicationProtocol
    trait Memory extends CommunicationProtocol
  """

  describe("ScalaTropy program"):
    describe("when architectural constraints and communication protocol are coherent"):
      it("should compile with just two peer instances"):
        commonImports + """
        type Client <: { type Tie <: via[MQTT toSingle Server] }
        type Server <: { type Tie <: via[MQTT toSingle Client] }

        def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
          for
            v <- on[Client]("Hello, Server!".pure)
            _ <- comm[Client, Server](v)
          yield ()
        """ should compile

      it("should compile with more peer instances and multiple links thanks to & composition"):
        commonImports + """
        type Client <: { type Tie <: via[MQTT toSingle Server] }
        type Server <: { type Tie <: via[MQTT toSingle Client] & via[Memory toMultiple Database] }
        type Database <: { type Tie <: via[Memory toMultiple Server] }
        
        def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
          for
            v <- on[Client]("Hello, Server!".pure)
            _ <- comm[Client, Server](v)
          yield ()
        """ should compile

    describe("when architectural constraints are violated"):
      it("should not compile"):
        val compileErrors: List[Error] = typeCheckErrors(commonImports + """
          type Client <: { type Tie <: via[MQTT toSingle Server] }
          type Server <: { type Tie <: via[MQTT toMultiple Client] }
        
          def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
            for
              v <- on[Client]("Hello, Server!".pure)
              _ <- comm[Client, Server](v)
            yield ()
        """)
        compileErrors should have size 1
        compileErrors.head.message should include:
          "Server does not conform to upper bound it.unibo.pslab.peers.PeersV2.TiedWithSingle[Client]"

    describe("when communication protocol are incoherent"):
      ignore("should not compile"):
        val compileErrors: List[Error] = typeCheckErrors(commonImports + """
          type Client <: { type Tie <: via[MQTT toSingle Server] }
          type Server <: { type Tie <: via[Memory toSingle Client] & via[MQTT toMultiple Database] }
          type Database <: { type Tie <: via[MQTT toMultiple Server] }
        
          def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
            for
              v <- on[Client]("Hello, Server!".pure)
              _ <- comm[Client, Server](v)
            yield ()
        """)
        compileErrors should have size 1
        compileErrors.head.message should include("no common communication protocol found")

    describe("when multiple links between two peer types exists"):
      it("should not compile"):
        val compileErrors: List[Error] = typeCheckErrors(commonImports + """
          type Client <: { type Tie <: via[MQTT toSingle Server] }
          type Server <: { type Tie <: via[Memory toSingle Client] & via[MQTT toSingle Client] }
          
          def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
            for
              v <- on[Client]("Hello, Server!".pure)
              _ <- comm[Client, Server](v)
            yield ()
        """)
        compileErrors should have size 1
        compileErrors.head.message should include("No more than one link can exists between two peer types")
