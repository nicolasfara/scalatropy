package it.unibo.pslab

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.{ Environment, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.{ Monad, MonadThrow }
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import MainWorker.*

object MainWorker:
  type Main <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Main] }

  case class Task(x: Int) derives ReadWriter:
    def compute: Int = x * x

  def mainWorkerProgram[F[_]: {MonadThrow, Console}](using lang: MultiParty[F]): F[Unit] =
    for
      task <- on[Main]:
        for
          peers <- reachablePeers[Worker]
          allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toList.toMap
          message <- anisotropicMessage[Main, Worker](allocation, Task(0))
        yield message
      taskOnWorker <- anisotropicComm[Main, Worker](task)
      _ <- on[Worker]:
        for
          t <- take(taskOnWorker)
          _ <- F.println(s"Worker received task with input ${t.x}, computed result: ${t.compute}")
        yield ()
    yield ()

object MainServer extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, Main]()
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = mainWorkerProgram[IO](using summon[Monad[IO]], summon[Console[IO]], lang)
      program

object Worker1 extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, Worker]()
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = mainWorkerProgram[IO](using summon[Monad[IO]], summon[Console[IO]], lang)
      program

object Worker2 extends IOApp.Simple:
  override def run: IO[Unit] = MqttNetwork
    .localBroker[IO, Worker]()
    .use: network =>
      val env = Environment.make[IO]
      val lang = MultiParty.make(env, network)
      val program = mainWorkerProgram[IO](using summon[Monad[IO]], summon[Console[IO]], lang)
      program
