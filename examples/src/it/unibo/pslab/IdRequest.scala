package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.TieTo.*

import java.util.UUID
import scala.util.Random
import it.unibo.pslab.multiparty.MultiPartyLanguage.*
import it.unibo.pslab.peers.Peers.Peer

object IdRequest:
  type IdProvider <: TieToMultiple[IdRequester]
  type IdRequester <: TieToSingle[IdProvider]

  private def assignTask[Local <: Peer : LocalPeer](requests: Map[Remote[?], UUID]): MultiParty[PerPeer[Int]] =
    val assignments = requests.map
    forEachPeer[IdProvider, IdRequester](requests.map(_._1 -> Random.nextInt()))

  def idRequestProgram[Local <: Peer : LocalPeer]: MultiParty[Unit] = for
    cid <- on[IdRequester](UUID.randomUUID())
    cidOnProvider <- comm[IdRequester, IdProvider](cid)
    assignedId <- on[IdProvider]:
      for
        cidMap <- asLocalAll(cidOnProvider)
        tasks <- assignTask(cidMap)
      yield tasks
    assignedIdOnRequester <- commPerPeer[IdProvider, IdRequester](assignedId)
    _ <- on[IdRequester]:
      for id <- asLocal(assignedIdOnRequester)
      yield println(s"IdRequester received assigned ID: $id")
  yield ()
