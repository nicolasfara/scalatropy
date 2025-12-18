package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.Multiplicity.*

import java.util.UUID

object IdRequest:
  type IdProvider <: { type Tie <: Multiple[IdRequester] }
  type IdRequester <: { type Tie <: Single[IdProvider] }

  def idRequestProgram: MultiParty[Unit] = for
    cid <- placed[UUID, IdRequester](UUID.randomUUID())
    cidOnProvider <- funnel[UUID, IdRequester, IdProvider](cid)
    assignedId <- placed[Selected[Int], IdProvider]:
      for 
        requests <- awaitAll(cidOnProvider)
        assigned <- selectProviders[Int, IdProvider, IdRequester](Map.empty)
      yield assigned
    assignedIdOnRequester <- selectiveMulticast[Int, IdProvider, IdRequester](assignedId)
    _ <- placed[Unit, IdRequester]:
      for id <- await(assignedIdOnRequester)
      yield println(s"IdRequester received assigned ID: $id")
  yield ()
