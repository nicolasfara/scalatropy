package it.unibo.pslab

import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
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
import upickle.default.ReadWriter

import MasterWorker.*

object MasterWorker:
  type Master <: { type Tie <: via[AnyProtocol toMultiple Worker] }
  type Worker <: { type Tie <: via[AnyProtocol toSingle Master] }

  case class Task(x: Int) derives ReadWriter:
    def compute: Int = x * x

  def masterWorkerProgram[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] =
    for
      tasks <- on[Master]:
        for
          peers <- reachablePeers[Worker]
          allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toList.toMap
          message <- anisotropicMessage[Master, Worker](allocation, Task(0))
        yield message
      taskOnWorker <- anisotropicComm[Master, Worker](tasks)
      partialResult <- on[Worker]:
        for
          t <- take(taskOnWorker)
          res <- t.compute.pure[F]
          _ <- F.println(s"Worker received task with input ${t.x}, computed result: $res")
        yield res
      allResults <- coAnisotropicComm[Worker, Master](partialResult)
      _ <- on[Master]:
        for
          resultsMap <- takeAll(allResults)
          result = resultsMap.values.sum
          _ <- F.println(s"Master collected results from workers: ${result}")
        yield ()
    yield ()

object MasterWorkerApp extends IOApp.Simple:
  override def run: IO[Unit] = List(MasterServer.run, Worker1.run, Worker2.run).parSequence_

object MasterServer extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Master](Configuration(appId = "masterworker"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(masterWorkerProgram[IO]).projectedOn[Master]:
        tiedTo[Worker] via mqtt

object Worker1 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "masterworker"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(masterWorkerProgram[IO]).projectedOn[Worker]:
        tiedTo[Master] via mqtt

object Worker2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Worker](Configuration(appId = "masterworker"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(masterWorkerProgram[IO]).projectedOn[Worker]:
        tiedTo[Master] via mqtt
