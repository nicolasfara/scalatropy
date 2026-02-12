package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*

import PingPong.*

object PingPong:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using lang: MultiParty[F]): F[Unit] =
    for
      initial <- on[Pinger](0.pure)
      _ <- pingPong(initial)
    yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](using lang: MultiParty[F])(initial: Int on Pinger): F[Unit] =
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

object Pinger extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Pinger](Configuration(appId = "pingpong"))
    ScalaTropy(pingPongProgram[IO]).projectedOn[Pinger](using mqttNetwork)

object Ponger extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Ponger](Configuration(appId = "pingpong"))
    ScalaTropy(pingPongProgram[IO]).projectedOn[Ponger](using mqttNetwork)
