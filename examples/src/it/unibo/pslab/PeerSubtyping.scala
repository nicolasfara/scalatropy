package it.unibo.pslab

import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.peers.Peers.*
import cats.MonadThrow
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.UpickleCodable.given

import cats.syntax.all.*
import cats.effect.IOApp
import cats.effect.IO
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.PeerSubtyping.*
import cats.effect.std.Console

object PeerSubtyping:
  type Device <: { type Tie <: via[AnyProtocol toSingle Server] }
  type Server <: { type Tie <: via[AnyProtocol toMultiple Device] }
  type Light <: Device
  type Thermometer <: Device

  def program[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] = for
    tempValue <- on[Device](10.pure)
    diobestia <- on[Thermometer](11.pure)
    diocane <- on[Server](12.pure)
    bestemmiaOnDevice <- isotropicComm[Server, Device](diocane)
    _ <- on[Device]:
      take(bestemmiaOnDevice) >>= (bestemmia => F.println(s"Bestemmia on device: $bestemmia"))
    tempOnServer <- isotropicComm[Server, Thermometer](diocane)
    _ <- on[Thermometer]:
      take(tempOnServer) >>= (temp => F.println(s"Temperature on server: $temp"))
  yield ()

object PeerSubtypingLaunchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    List(ServerApp.run, LightApp.run, ThermometerApp.run).parSequence_

object ServerApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use:
      mqtt =>
        ScalaTropy(program[IO]).projectedOn[Server]:
          tiedTo[Light] via mqtt
          tiedTo[Thermometer] via mqtt
          // tiedTo[Device] via mqtt

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
