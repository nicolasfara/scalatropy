package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.Multiplicity.*

object MainWorker:
  type Main <: { type Tie <: Multiple[Worker] }
  type Worker <: { type Tie <: Single[Main] }

  case class Task(x: Int):
    def compute: Int = x * x

  def mainWorkerProgram: MultiParty[Unit] = ??? // for
//    workerId <- placed[Int, Worker](42)
//    workerIdOnMain <- unicast[Int, Worker, Main](workerId)
//    allocation <- placed[List[(Int, Task)], Main]:
//      awaitAll(workerIdOnMain).map(ids => ids.map(id => id -> Task(id)).toList)
//    allocationOnWorkers <- multicast[List[(Int, Task)], Main, Worker](allocation)
//    result <- placed[List[Int], Worker]:
//      for
//        task <- await(allocationOnWorkers)
//        localId <- await(workerId)
//      yield task.filter(_._1 == localId).map(t => t._2.compute)
//    resultOnMain <- unicast[List[Int], Worker, Main](result)
//    finalResult <- placed[Unit, Main](awaitAll(resultOnMain).map(_.flatten))
//  yield println(s"Final result: $finalResult")
