package it.unibo.pslab.matmul

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.matmul.LinearAlgebra.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.NetworkMonitor.withCsvMonitoring
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.{ Monad, MonadThrow }
import cats.data.NonEmptyList
import cats.effect.{ ExitCode, IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import MatMulMasterWorker.*

object MatMulMasterWorker:
  type Master <: { type Tie <: via[AnyProtocol toMultiple Worker] }
  type Worker <: { type Tie <: via[AnyProtocol toSingle Master] }

  case class PartialResult(chunk: VecChunk[Double]) derives ReadWriter

  case class Task(chunk: MatrixChunk[Double], vector: Vec[Double]) derives ReadWriter:
    def compute: PartialResult =
      PartialResult(VecChunk(chunk.startRow, chunk.endRow, chunk.subMatrix * vector))

  object Task:
    def nil: Task = Task(MatrixChunk(0, 0, Matrix(Nil)), Vec(Nil))

  def matmul[F[_]: {MonadThrow, Console}](using lang: MultiParty[F]): F[Unit] =
    for
      matrix <- on[Master](Matrix.product(50, 50).pure)
      vector <- on[Master](take(matrix).map(_.cols).map(Vec.naturals))
      _ <- impl(matrix, vector)
    yield ()

  private def impl[F[_]: {MonadThrow, Console}](using
      MultiParty[F],
  )(matrix: Matrix[Double] on Master, vector: Vec[Double] on Master): F[Unit] =
    for
      tasks <- on[Master]:
        for
          m <- take(matrix)
          v <- take(vector)
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
          result = resultsMap.values.toList.sortBy(_.chunk.startRow) >>= (_.chunk.values.values)
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
            worker -> Task(MatrixChunk(startRow, endRow, matrix.rowSlice(startRow, endRow)), vector)
          .toList
          .toMap
          .pure

object MatMulApp extends IOApp:

  override def run(args: List[String]): IO[ExitCode] =
    args match
      case List(numWorkersStr, label) =>
        val numWorkers = numWorkersStr.toInt
        val choreo = master(label) +: List.fill(numWorkers)(worker)
        choreo.parSequence_.as(ExitCode.Success)
      case _ => IO.println("Usage: MatMulMaster <num-workers> <label>").as(ExitCode.Error)

  def master(label: String): IO[Unit] =
    withCsvMonitoring(s"evaluation/selective-experiment-$label.csv"):
      MqttNetwork.fromEnv[IO, Master](Configuration(appId = "matmul"))
    .use: mqtt =>
      ScalaTropy(matmul[IO]).projectedOn[Master]:
        tiedTo[Worker] via mqtt

  def worker: IO[Unit] =
    val mqttNetwork = MqttNetwork.fromEnv[IO, Worker](Configuration(appId = "matmul"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(matmul[IO]).projectedOn[Worker]:
        tiedTo[Master] via mqtt
