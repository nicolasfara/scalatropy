package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.*
import it.unibo.pslab.multiparty.Language.*
import it.unibo.pslab.peers.Peers.Peer

object MainWorker:
  type Main <: TieToMultiple[Worker]
  type Worker <: TieToSingle[Main]

  private case class Task(x: Int):
    def compute: Int = x * x

  def mainWorkerProgram[Local <: Peer : LocalPeer]: MultiParty[Unit] = for
    task <- on[Main]:
      for
        peers <- remotes[Worker]
        allocation = peers.map(_ -> Task(scala.util.Random.nextInt(100))).toMap
        message <- forEachPeer[Main, Worker](allocation)
      yield message
    taskOnWorker <- commPerPeer[Main, Worker](task)
    _ <- on[Worker]:
      for t <- asLocal(taskOnWorker)
      yield println(s"Worker received task with input ${t.x}, computed result: ${t.compute}")
  yield ()
