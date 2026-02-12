package it.unibo.pslab

import it.unibo.pslab.multiparty.{ Environment, MultiParty }
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.Monad
import cats.effect.kernel.{ MonadCancel, Resource }

object ScalaTropy:

  opaque type ScalaTropy[F[_], Result] = Impl[F, Result]

  def apply[F[_]: Monad, Result](program: MultiParty[F] ?=> F[Result]): ScalaTropy[F, Result] = Impl(program)

  extension [F[_], Result](trope: ScalaTropy[F, Result])
    def projectedOn[Local <: Peer: PeerTag](using
        networkRes: Resource[F, Network[F, Local]],
    )(using MonadCancel[F, Throwable]) =
      networkRes.use: net =>
        val env = Environment.make[F]
        given MultiParty[F] = MultiParty.make(env, net)
        trope.program

  private class Impl[F[_]: Monad, Result](val program: MultiParty[F] ?=> F[Result])
