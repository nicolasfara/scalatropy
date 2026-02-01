package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.{ Environment, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import TrianglePingPong.*

object TrianglePingPong:
  type Alice <: { type Tie <: Single[Bob] & Single[Andromeda] }
  type Bob <: { type Tie <: Single[Alice] & Single[Andromeda] }
  type Andromeda <: { type Tie <: Single[Bob] & Single[Alice] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using lang: MultiParty[F]): F[Unit] = for
    initial <- on[Alice](0.pure)
    _ <- pingPong(initial)
  yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](initial: Int on Alice)(using lang: MultiParty[F]): F[Unit] = for
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

object Bob extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, TrianglePingPong.Bob](Configuration(appId = "triangle-pingpong"))
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], summon[Temporal[IO]], lang)
      program

object Andromeda extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, TrianglePingPong.Andromeda](Configuration(appId = "triangle-pingpong"))
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], summon[Temporal[IO]], lang)
      program

object Alice extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, TrianglePingPong.Alice](Configuration(appId = "triangle-pingpong"))
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], summon[Temporal[IO]], lang)
      program
