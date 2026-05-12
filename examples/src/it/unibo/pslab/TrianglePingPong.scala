package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import TrianglePingPong.*

object TrianglePingPong:
  type Alice <: { type Tie <: via[AnyProtocol toSingle Bob] & via[AnyProtocol toSingle Andromeda] }
  type Bob <: { type Tie <: via[AnyProtocol toSingle Alice] & via[AnyProtocol toSingle Andromeda] }
  type Andromeda <: { type Tie <: via[AnyProtocol toSingle Bob] & via[AnyProtocol toSingle Alice] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using MultiParty[F]): F[Unit] =
    for
      initial <- on[Alice](0.pure)
      _ <- pingPong(initial)
    yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](initial: Int on Alice)(using MultiParty[F]): F[Unit] =
    for
      _ <- on[Alice]:
        for
          v <- take(initial)
          _ <- F.println(s"Alice's value: $v")
        yield ()
      aliceSendToBob <- comm[Alice, Bob](initial)
      prepareMessageToAndromeda <- on[Bob]:
        for
          v <- take(aliceSendToBob)
          _ <- F.println(s"Bob's value: $v")
        yield v + 1
      bobSendToAndromeda <- comm[Bob, Andromeda](prepareMessageToAndromeda)
      prepareMessageToAlice <- on[Andromeda]:
        for
          v <- take(bobSendToAndromeda)
          _ <- F.println(s"Andromeda's value: $v")
        yield v + 1
      pingerSum <- comm[Andromeda, Alice](prepareMessageToAlice)
      _ <- F.sleep(1.second)
      _ <- pingPong(pingerSum)
    yield ()

object TrianglePingPongApp extends IOApp.Simple:
  override def run: IO[Unit] = List(Bob.run, Andromeda.run, Alice.run).parSequence_

object Bob extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Bob](Configuration(appId = "3-ping-pong"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(pingPongProgram[IO]).projectedOn[Bob]:
        tiedTo[Alice] via mqtt
        tiedTo[Andromeda] via mqtt

object Andromeda extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Andromeda](Configuration(appId = "3-ping-pong"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(pingPongProgram[IO]).projectedOn[Andromeda]:
        tiedTo[Alice] via mqtt
        tiedTo[Bob] via mqtt

object Alice extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Alice](Configuration(appId = "3-ping-pong"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(pingPongProgram[IO]).projectedOn[Alice]:
        tiedTo[Bob] via mqtt
        tiedTo[Andromeda] via mqtt
