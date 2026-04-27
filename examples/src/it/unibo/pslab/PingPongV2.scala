package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.ScalaTropyV2.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiPartyV2
import it.unibo.pslab.multiparty.MultiPartyV2.*
import it.unibo.pslab.network.IoT
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.PeersV2.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*

import PingPongV2.*

object PingPongV2:
  type Pinger <: { type Tie <: via[IoT toSingle Ponger] }
  type Ponger <: { type Tie <: via[IoT toSingle Pinger] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using MultiPartyV2[F]): F[Unit] =
    for
      initial <- on[Pinger](0.pure)
      _ <- pingPong(initial)
    yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](using MultiPartyV2[F])(initial: Int on Pinger): F[Unit] =
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

object PingerV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Pinger](Configuration(appId = "pingpong"))
      .use: mqttNet =>
        ScalaTropyV2(pingPongProgram[IO]).projectedOn[Pinger]:
          tiedTo[Ponger] via mqttNet

object PongerV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Ponger](Configuration(appId = "pingpong"))
      .use: mqttNet =>
        ScalaTropyV2(pingPongProgram[IO]).projectedOn[Ponger]:
          tiedTo[Pinger] via mqttNet
