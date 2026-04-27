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

    trait MQTTNetwork[F[_], P <: Peer] extends Network[F, P] with MQTT
    trait WebSocketNetwork[F[_], P <: Peer] extends Network[F, P] with WebSocket

    type Pinger <: { type Tie <: via[MQTT toSingle Ponger] }
    type Ponger <: { type Tie <: via[MQTT toSingle Pinger] }

    def app[F[_]: Monad](using MultiPartyV2[F]): F[Unit] = ???

    val mqttNet: MQTTNetwork[IO, Pinger] = ???
    val wsNet: WebSocketNetwork[IO, Pinger] = ???
  """

  describe("Deployment definition"):
    describe("when is coherent with architectural definition"):
      it("should compile"):
        commonCode + """
        ScalaTropyV2(app[IO]).projectedOn[Pinger]:
          tiedTo[Ponger] via mqttNet
        """ should compile

    describe("when is not coherent with architectural definition"):
      describe("because of wrong communication protocol"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          ScalaTropyV2(app[IO]).projectedOn[Pinger]:
            tiedTo[Ponger] via wsNet
          """)
          compileErrors should have size 1
          compileErrors.head.message should include:
            "Cannot prove that Pinger <:< it.unibo.pslab.peers.PeersV2.TiedWithComm[Ponger, Protocol]"

      describe("because of wrong tie"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          type AnotherPeer <: { type Tie <: via[MQTT toSingle Pinger] }

          ScalaTropyV2(app[IO]).projectedOn[Pinger]:
            tiedTo[AnotherPeer] via mqttNet
          """)
          compileErrors should have size 1
          compileErrors.head.message should (
            include:
              """|No given instance of type it.unibo.pslab.deployment.Deployment.Scope[F, Local, PeerId]
                 |was found for parameter deployment of method tiedTo
                 |""".stripMargin.replaceAll("\\s+", " ")
            and include:
              "Local  is a type variable with constraint <: it.unibo.pslab.peers.PeersV2.TiedTo[AnotherPeer]"
          )

      describe("because of using a network placed on a peer that is not the local one"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          val mqttNetworkOnAlice: MQTTNetwork[IO, Ponger] = ???

          ScalaTropyV2(app[IO]).projectedOn[Pinger]:
            tiedTo[Ponger] via mqttNetworkOnAlice
          """)
          compileErrors should have size 1
          compileErrors.head.message should include:
            "Required: it.unibo.pslab.network.NetworkManager[cats.effect.IO, Pinger, PeerId]"
