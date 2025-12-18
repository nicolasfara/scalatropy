package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.{TieToMultiple, TieToSingle}

object MainWorker:
  type Main <: TieToMultiple[Worker]
  type Worker <: TieToSingle[Main]

  private case class Task(x: Int):
    def compute: Int = x * x

  def mainWorkerProgram: MultiParty[Unit] = for
    task <- placed[PerPeer[Task], Main]:
      for
        peers <- remotes[Worker]()
        allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toMap
        message <- forEachPeer[Task, Main, Worker](allocation)
      yield message
    taskOnWorker <- commPerPeer[Task, Main, Worker](task)
    _ <- placed[Unit, Worker]:
      for t <- await(taskOnWorker)
      yield println(s"Worker received task with input ${t.x}, computed result: ${t.compute}")
  yield ()
