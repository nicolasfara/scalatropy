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
import cats.effect.std.Console
import cats.syntax.all.*

object PeerSubtyping:
  type Device <: { type Tie <: via[AnyProtocol toSingle Server] }
  type Server <: { type Tie <: via[AnyProtocol toMultiple Device] }
  type Light <: Device
  type Thermometer <: Device

  def program[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] = for
    tempValue <- on[Device](10.pure)
    diobestia <- on[Thermometer](10.pure)
    diocane <- on[Server](10.pure)
    bestemmiaOnDevice <- isotropicComm[Server, Device](diocane)
    _ <- on[Device]:
      take(bestemmiaOnDevice) >>= (value => F.println(s"Device received bestemmia from Server: ${value}"))
    tempOnServer <- isotropicComm[Server, Thermometer](diocane)
    _ <- on[Thermometer]:
      take(tempOnServer) >>= (value => F.println(s"Server received temperature from Thermometer: ${value}"))
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
