package it.unibo.pslab

import cats.syntax.all.*
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.{TieToMultiple, TieToSingle}

import java.util.UUID
import scala.util.Random

object IdRequest:
  type IdProvider <: TieToMultiple[IdRequester]
  type IdRequester <: TieToSingle[IdProvider]

  private def assignTask(requests: Map[Remote[?], UUID]): MultiParty[PerPeer[Int]] =
    val assignments = requests.map
    forEachPeer[Int, IdProvider, IdRequester](requests.map(_._1 -> Random.nextInt()))

  def idRequestProgram: MultiParty[Unit] = for
    cid <- placed[UUID, IdRequester](UUID.randomUUID())
    cidOnProvider <- comm[UUID, IdRequester, IdProvider](cid): MultiParty[Many[UUID] on IdProvider]
    assignedId <- placed[PerPeer[Int], IdProvider]:
      awaitAll(cidOnProvider) >>= assignTask
    assignedIdOnRequester <- commPerPeer[Int, IdProvider, IdRequester](assignedId)
    _ <- placed[Unit, IdRequester]:
      for id <- await(assignedIdOnRequester)
      yield println(s"IdRequester received assigned ID: $id")
  yield ()
