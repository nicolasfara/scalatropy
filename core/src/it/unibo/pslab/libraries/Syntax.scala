package it.unibo.pslab.libraries

import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.{ on, take }
import it.unibo.pslab.peers.Peers.Peer

import cats.Monad
import cats.syntax.all.*

object Syntax:

  def take2[Local <: Peer](using
      Label[Local],
  )[F[_]: Monad, V1, V2](using MultiParty[F])(placed: (V1 on Local, V2 on Local)): F[(V1, V2)] =
    for
      v1 <- take(placed._1)
      v2 <- take(placed._2)
    yield (v1, v2)
