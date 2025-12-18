package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.Multiplicity.*

object Multiparty:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }
  
  def pingPongProgram: MultiParty[Unit] = for 
    initial <- placed[Int, Pinger](0)
    _ <- pingPong(initial)
  yield ()

  def pingPong(initial: Int on Pinger): MultiParty[Unit] = for
    onPonger <- unicast[Int, Pinger, Ponger](initial)
    newCounter <- placed[Int, Ponger]:
      await(onPonger).map(_ + 1)
    newCounterOnPinger <- unicast[Int, Ponger, Pinger](newCounter)
  yield pingPong(newCounterOnPinger)

@main
def main(): Unit =
    println("Multiparty Ping-Pong example defined.")
    val program = Multiparty.pingPongProgram
    println(program)