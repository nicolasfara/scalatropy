package it.unibo.pslab.matmul

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.network.NetworkMonitor.withCsvMonitoring
import it.unibo.pslab.network.memory.InMemoryNetwork

import cats.data.NonEmptyList
import cats.effect.{ ExitCode, IO, IOApp, Resource }
import cats.syntax.all.*

/*
 * These two main objects run the evaluation of the matmul implementation using both the selective and the broadcasting
 * communication styles.
 * 
 * They are implemented to be run with the in-memory network in order to be able to run a high number of workers
 * without the overhead of spawning multiple JVMs and IO Apps.
 */

object MatMulMaster extends IOApp:
  import MatMulMasterWorker.*

  override def run(args: List[String]): IO[ExitCode] =
    args match
      case List(numWorkersStr, label) =>
        val numWorkers = numWorkersStr.toInt
        runMatMul(numWorkers, label).as(ExitCode.Success)
      case _ => IO.println("Usage: MatMulMaster <num-workers> <label>").as(ExitCode.Error)

  private def runMatMul(numWorkers: Int, label: String): IO[Unit] =
    val masterAddr = InMemoryNetwork.Address[Master]("master")
    val workerAddrs = (1 to numWorkers).map(i => InMemoryNetwork.Address[Worker](s"worker-$i")).toList
    val allPeerAddrs = NonEmptyList(masterAddr, workerAddrs)

    val networksResource = Resource
      .eval(InMemoryNetwork.messagesDispatcher[IO])
      .flatMap: dispatcher =>
        val masterNet = withCsvMonitoring(s"evaluation/selective-experiment-$label.csv"):
          InMemoryNetwork[IO, Master](masterAddr.id, allPeerAddrs, dispatcher)

        val workerNets = workerAddrs.traverse: workerAddr =>
          InMemoryNetwork[IO, Worker](workerAddr.id, allPeerAddrs, dispatcher)

        (masterNet, workerNets).tupled

    networksResource.use: (masterNet, workerNets) =>
      (
        ScalaTropy(matmul[IO]).projectedOn[Master](using masterNet),
        ScalaTropy(matmul[IO]).projectedOnMultiple[Worker](using workerNets),
      ).parTupled.void

object InefficientMatMulMaster extends IOApp:
  import BroadcastBasedMatMulMasterWorker.*

  override def run(args: List[String]): IO[ExitCode] =
    args match
      case List(numWorkersStr, label) =>
        val numWorkers = numWorkersStr.toInt
        runMatMul(numWorkers, label).as(ExitCode.Success)
      case _ => IO.println("Usage: InefficientMatMulMaster <num-workers> <label>").as(ExitCode.Error)

  private def runMatMul(numWorkers: Int, label: String): IO[Unit] =
    val masterAddr = InMemoryNetwork.Address[Master]("master")
    val workerAddrs = (1 to numWorkers).map(i => InMemoryNetwork.Address[Worker](s"worker-$i")).toList
    val allPeers = NonEmptyList(masterAddr, workerAddrs)

    val networksResource = Resource
      .eval(InMemoryNetwork.messagesDispatcher[IO])
      .flatMap: dispatcher =>
        val masterNet = withCsvMonitoring(s"evaluation/broadcasting-experiment-$label.csv"):
          InMemoryNetwork[IO, Master]("master", allPeers, dispatcher)

        val workerNets = workerAddrs.traverse: workerAddr =>
          InMemoryNetwork[IO, Worker](workerAddr.id, allPeers, dispatcher)

        (masterNet, workerNets).tupled

    networksResource.use: (masterNet, workerNets) =>
      (
        ScalaTropy(matmul[IO]).projectedOn[Master](using masterNet),
        ScalaTropy(matmul[IO]).projectedOnMultiple[Worker](using workerNets),
      ).parTupled.void
