package it.unibo.pslab

import it.unibo.pslab.PeerSubtyping.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.MonadThrow
import cats.effect.{ IO, IOApp }
import cats.effect.std.{ Console, Random }
import cats.syntax.all.*

object PeerSubtyping:
  type Device <: { type Tie <: via[AnyProtocol toSingle Server] }
  type Server <: { type Tie <: via[AnyProtocol toMultiple Device] }
  type Light <: Device
  type Thermometer <: Device

  def program[F[_]: {MonadThrow, Console, Random}](using MultiParty[F]): F[Unit] = for
    fakeIdDevice <- on[Device] {
      F.betweenDouble(0.0, 10.0) flatTap log("Generated device ID: ")
    }
    temperature <- on[Thermometer](F.nextInt)
    _ <- on[Thermometer] {
      take(fakeIdDevice) >>= log("On on subtype, I can access value on supertype: ")
    }
    _ <- on[Device] {
      take(fakeIdDevice) >>= log("Anything that is a `Device` will execute this code block: ")
    }
    tempOnServer <- coAnisotropicComm[Thermometer, Server](temperature)
    _ <- on[Server] {
      takeAll(tempOnServer) >>= log("Temperatures: ")
    }
  yield ()

object PeerSubtypingLaunchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    List(ServerApp.run, LightApp.run, ThermometerApp.run).parSequence_

object ServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(program[IO]).projectedOn[Server]:
        tiedTo[Device] via mqtt

object LightApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(program[IO]).projectedOn[Light]:
        tiedTo[Server] via mqtt

object ThermometerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Thermometer](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(program[IO]).projectedOn[Thermometer]:
        tiedTo[Server] via mqtt
