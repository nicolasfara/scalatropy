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

import PingPong.*

object PingPong:
  type Pinger <: { type Tie <: via[AnyProtocol toSingle Ponger] }
  type Ponger <: { type Tie <: via[AnyProtocol toSingle Pinger] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using MultiParty[F]): F[Unit] =
    for
      initial <- on[Pinger](0.pure)
      _ <- pingPong(initial)
    yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](using MultiParty[F])(initial: Int on Pinger): F[Unit] =
    for
      onPonger <- comm[Pinger, Ponger](initial)
      newCounter <- on[Ponger]:
        for
          v <- take(onPonger)
          _ <- F.println(s"Ponger received value: $v")
        yield v + 1
      newCounterOnPinger <- comm[Ponger, Pinger](newCounter)
      result <- on[Pinger]:
        for
          v <- take(newCounterOnPinger)
          _ <- F.println(s"Pinger received value: $v")
        yield v + 1
      _ <- F.sleep(1.second)
      _ <- pingPong(result)
    yield ()

object PingPongApp extends IOApp.Simple:
  override def run: IO[Unit] = List(Pinger.run, Ponger.run).parSequence_

object Pinger extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Pinger](Configuration(appId = "ping-pong"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(pingPongProgram[IO]).projectedOn[Pinger]:
        tiedTo[Ponger] via mqtt

object Ponger extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Ponger](Configuration(appId = "ping-pong"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(pingPongProgram[IO]).projectedOn[Ponger]:
        tiedTo[Pinger] via mqtt
