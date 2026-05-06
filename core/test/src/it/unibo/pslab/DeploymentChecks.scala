package it.unibo.pslab

import scala.compiletime.testing.{ typeCheckErrors, Error }

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should

class DeploymentChecks extends AnyFunSpec with should.Matchers:

  inline val commonCode = """
    import _root_.it.unibo.pslab.ScalaTropyV2.*
    import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    import _root_.it.unibo.pslab.multiparty.MultiPartyV2.*
    import _root_.it.unibo.pslab.peers.PeersV2.*
    import _root_.it.unibo.pslab.network.{ Network, CommunicationProtocol }
    import cats.effect.IO
    import cats.Monad

    trait MQTT extends CommunicationProtocol
    trait WebSocket extends CommunicationProtocol

    trait MQTTNetwork[F[_], P <: Peer] extends Network[F, P] with MQTT:
      type PeerId[P <: Peer] = Int
    trait WebSocketNetwork[F[_], P <: Peer] extends Network[F, P] with WebSocket:
      type PeerId[P <: Peer] = Int

    type A <: { type Tie <: via[MQTT toSingle B] & via[MQTT toSingle C] }
    type B <: { type Tie <: via[MQTT toSingle A] & via[MQTT toSingle C] }
    type C <: { type Tie <: via[MQTT toSingle A] & via[MQTT toSingle B] }

    def app[F[_]: Monad](using MultiPartyV2[F]): F[Unit] = ???

    val mqttNet: MQTTNetwork[IO, A] = ???
    val wsNet: WebSocketNetwork[IO, A] = ???
  """

  describe("Deployment definition"):
    describe("when is coherent with architectural definition"):
      it("should compile"):
        commonCode + """
        ScalaTropyV2(app[IO]).projectedOn[A]:
          tiedTo[B] via mqttNet
          tiedTo[C] via mqttNet
        """ should compile

    describe("when is not coherent with architectural definition"):
      describe("because of wrong communication protocol"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropyV2(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNet
            tiedTo[C] via wsNet
          """)
          compileErrors should have size 1
          println(compileErrors.head.message)
          compileErrors.head.message should include:
            "Cannot prove that A <:< it.unibo.pslab.peers.PeersV2.TiedWithComm[C, Protocol]"

      describe("because of wrong tie"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropyV2(app[IO]).projectedOn[A]:
            tiedTo[A] via mqttNet
          """)
          compileErrors should have size 1
          compileErrors.head.message should (
            include:
              """|No given instance of type it.unibo.pslab.deployment.Deployment.Scope[F, Local, PeerId]
                 |was found for parameter deployment of method tiedTo
                 |""".stripMargin.replaceAll("\\s+", " ")
            and include:
              "Local  is a type variable with constraint <: it.unibo.pslab.peers.PeersV2.TiedTo[A]"
          )

      describe("because of using a network placed on a peer that is not the local one"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          val mqttNetworkOnB: MQTTNetwork[IO, B] = ???

          ScalaTropyV2(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNetworkOnB
          """)
          compileErrors should have size 1
          compileErrors.head.message should include:
            "Required: it.unibo.pslab.network.NetworkManager[cats.effect.IO, A, PeerId]"

      describe("because of incomplete ties"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropyV2(app[IO]).projectedOn[A]:
            tiedTo[B] via mqttNet
            // missing tie with C
          """)
          println(compileErrors.size)
          println(compileErrors)
          // compileErrors should have size 1
          // compileErrors.head.message should include:
