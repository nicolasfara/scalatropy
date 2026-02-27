package it.unibo.pslab.matmul

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.matmul.{ Matrix, Vec, VecChunk }
import it.unibo.pslab.matmul.LinearAlgebra.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.NetworkMonitor.withCsvMonitoring
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import BroadcastBasedMatMulMasterWorker.*

object BroadcastBasedMatMulMasterWorker:
  type Master <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Master] }

  case class PartialResult(chunk: VecChunk[Double]) derives ReadWriter

  // Differently from the efficient version, the task contains the whole matrix!
  case class Task(startRow: Int, endRow: Int, matrix: Matrix[Double], vector: Vec[Double]) derives ReadWriter:
    def compute: PartialResult = PartialResult(VecChunk(startRow, endRow, matrix.rowSlice(startRow, endRow) * vector))

  object Task:
    def nil: Task = Task(0, 0, Matrix(Nil), Vec(Nil))

  def matmul[F[_]: {MonadThrow, Console}](using lang: MultiParty[F]): F[Unit] =
    for
      matrix <- on[Master](Matrix.product(50, 50).pure)
      vector <- on[Master](take(matrix).map(_.cols).map(Vec.naturals))
      _ <- impl(matrix, vector)
    yield ()

  def impl[F[_]: {MonadThrow, Console}](using
      MultiParty[F],
  )(matrix: Matrix[Double] on Master, vector: Vec[Double] on Master): F[Unit] =
    for
      tasks <- on[Master]:
        for
          m <- take(matrix)
          v <- take(vector)
          _ <- F.println(s"Matrix:\n${m.show}\nVector: ${v.show}")
          workers <- reachablePeers[Worker]
          allocation <- allocate(m, v) to workers
          message <- anisotropicMessage[Master, Worker](allocation, Task.nil)
        yield message
      taskOnWorker <- anisotropicComm[Master, Worker](tasks)
      partialResult <- on[Worker]:
        take(taskOnWorker).map(_.compute)
      allResults <- coAnisotropicComm[Worker, Master](partialResult)
      _ <- on[Master]:
        for
          resultsMap <- takeAll(allResults)
          result = resultsMap.values.toList.sortBy(_.chunk.startRow).flatMap(_.chunk.values.values)
          _ <- F.println(s"Result: ${Vec(result).show}")
        yield ()
    yield ()

  trait Allocator[F[_]]:
    infix def to(using l: MultiParty[F])(workers: NonEmptyList[l.Remote[Worker]]): F[Map[l.Remote[Worker], Task]]

  def allocate[F[_]: Monad, T](matrix: Matrix[Double], vector: Vec[Double]): Allocator[F] =
    new Allocator[F]:
      infix def to(using l: MultiParty[F])(workers: NonEmptyList[l.Remote[Worker]]): F[Map[l.Remote[Worker], Task]] =
        val chunkSize = math.ceil(matrix.rows.toDouble / workers.size).toInt
        workers
          .mapWithIndex: (worker, index) =>
            val startRow = index * chunkSize
            val endRow = math.min(startRow + chunkSize, matrix.rows)
            worker -> Task(startRow, endRow, matrix, vector)
          .toList
          .toMap
          .pure

trait InefficientMatMulMqttConfig:
  val mqttConfig = Configuration(
    appId = "broadcast-matmul-master-worker",
    initialWaitWindow = 20.seconds,
  )

object InefficientMatMulMaster extends IOApp with InefficientMatMulMqttConfig:
  override def run(args: List[String]): IO[ExitCode] =
    args.headOption match
      case Some(label) =>
        withCsvMonitoring(s"evaluation/broadcasting-experiment-$label.csv"):
          val mqttNetwork = MqttNetwork.fromEnv[IO, Master](mqttConfig)
          ScalaTropy(matmul[IO]).projectedOn[Master](using mqttNetwork).as(ExitCode.Success)
      case None => IO.println("Usage: InefficientMatMulMaster <label>").as(ExitCode.Error)

object InefficientMatMulWorker extends IOApp.Simple with InefficientMatMulMqttConfig:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.fromEnv[IO, Worker](mqttConfig)
    ScalaTropy(matmul[IO]).projectedOn[Worker](using mqttNetwork)
