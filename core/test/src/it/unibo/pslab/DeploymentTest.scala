package it.unibo.pslab

import org.scalatest.matchers.should
import org.scalatest.funspec.AnyFunSpec
import scala.compiletime.testing.{ Error, typeCheckErrors }

class DeploymentTest extends AnyFunSpec with should.Matchers:

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
          compileErrors should not be empty
          println(s"Compile errors: ${compileErrors.map(_.message).mkString("\n")}")

      describe("because of wrong tie"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          type AnotherPeer <: { type Tie <: via[MQTT toSingle Pinger] }

          ScalaTropyV2(app[IO]).projectedOn[Pinger]:
            tiedTo[AnotherPeer] via mqttNet
          """)
          compileErrors should not be empty
          println(s"Compile errors: ${compileErrors.map(_.message).mkString("\n")}")

      describe("because of using a network placed on a peer that is not the local one"):
        it("should not compile"):
          val compileErrors: List[Error] = typeCheckErrors(commonCode + """
          val mqttNetworkOnAlice: MQTTNetwork[IO, Ponger] = ???

          ScalaTropyV2(app[IO]).projectedOn[Pinger]:
            tiedTo[Ponger] via mqttNetworkOnAlice
          """)
          compileErrors should not be empty
          println(s"Compile errors: ${compileErrors.map(_.message).mkString("\n")}")
