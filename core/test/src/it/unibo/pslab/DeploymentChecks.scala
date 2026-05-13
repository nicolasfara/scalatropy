package it.unibo.pslab

import scala.compiletime.testing.{ typeCheckErrors, Error }

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DeploymentChecks extends AnyFunSpec with should.Matchers:

  inline val commonCode = """
    import _root_.it.unibo.pslab.ScalaTropy.*
    import _root_.it.unibo.pslab.multiparty.MultiParty
    import _root_.it.unibo.pslab.multiparty.MultiParty.*
    import _root_.it.unibo.pslab.peers.Peers.*
    import _root_.it.unibo.pslab.network.{ Network, CommunicationProtocol }
    import cats.effect.IO
    import cats.Monad

    trait MQTT extends CommunicationProtocol
    trait WebSocket extends CommunicationProtocol

    trait MQTTNetwork[F[_], P <: Peer] extends Network[F, P, [_ <: Peer] =>> Int] with MQTT
    trait WebSocketNetwork[F[_], P <: Peer] extends Network[F, P, [_ <: Peer] =>> Int] with WebSocket

    type A <: { type Tie <: via[MQTT toSingle B] & via[MQTT toSingle C] }
    type B <: { type Tie <: via[MQTT toSingle A] & via[MQTT toSingle C] }
    type C <: { type Tie <: via[MQTT toSingle A] & via[MQTT toSingle B] }
    type D <: B

    def app[F[_]: Monad](using MultiParty[F]): F[Unit] = ???

    val mqttNet: MQTTNetwork[IO, A] = ???
    val wsNet: WebSocketNetwork[IO, A] = ???
  """

  describe("Deployment definition"):
    describe("when is coherent with architectural definition"):
      it("should compile"):
        commonCode + """
        ScalaTropy(app[IO]).projectedOn[A]:
          tiedTo[B] via mqttNet
          tiedTo[C] via mqttNet
        """ should compile

      it("should compile when a configured peer is a subtype of an architectural peer"):
        commonCode + """
        ScalaTropy(app[IO]).projectedOn[A]:
          tiedTo[D] via mqttNet
          tiedTo[C] via mqttNet
        """ should compile

    describe("when is not coherent with architectural definition"):
      describe("because of wrong communication protocol"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropy(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNet
            tiedTo[C] via wsNet
          """)
          compileErrors should have size 1
          println(compileErrors.head.message)
          compileErrors.head.message should include:
            "Cannot prove that A <:< it.unibo.pslab.peers.Peers.TiedWithComm[C, Protocol]"

      describe("because of wrong tie"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropy(app[IO]).projectedOn[A]:
            tiedTo[A] via mqttNet
          """)
          compileErrors should have size 1
          compileErrors.head.message should (
            include:
              """|No given instance of type it.unibo.pslab.deployment.Deployment.Scope[F, Local, PeerId]
                 |was found for parameter deployment of method tiedTo
                 |""".stripMargin.replaceAll("\\s+", " ")
            and include:
              "Local  is a type variable with constraint <: it.unibo.pslab.peers.Peers.TiedTo[A]"
          )

      describe("because of using a network placed on a peer that is not the local one"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          val mqttNetworkOnB: MQTTNetwork[IO, B] = ???

          ScalaTropy(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNetworkOnB
            tiedTo[C] via mqttNet
          """)
          compileErrors should have size 1
          compileErrors.head.message should include:
            "Required: it.unibo.pslab.network.Network[cats.effect.IO, A"

      describe("because of incomplete ties"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropy(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNet
            // missing tie with C
          """)
          compileErrors should have size 1
          compileErrors.head.message should include:
            "Mismatch between expected and configured tied peers"
