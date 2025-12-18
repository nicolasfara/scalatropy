package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.Multiplicity.*

object MainWorker:
  type Main <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Main] }

  private case class Task(x: Int):
    def compute: Int = x * x

  def mainWorkerProgram: MultiParty[Unit] = for
    task <- placed[Aniso[Task], Main]:
      for
        peers <- remotes[Worker]()
        allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toMap
        message <- selectProviders[Task, Main, Worker](allocation)(Task(-1))
      yield message
    taskOnWorker <- anisotropic[Task, Main, Worker](task)
    _ <- placed[Unit, Worker]:
      for t <- await(taskOnWorker)
      yield println(s"Worker received task with input ${t.x}, computed result: ${t.compute}")
  yield ()
