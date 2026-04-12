package it.unibo.pslab

import it.unibo.pslab.multiparty.{ Environment, MultiPartyV2 }
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.Monad
import cats.effect.kernel.{ MonadCancel, Resource }
import cats.implicits.toTraverseOps

object ScalaTropyV2:

  opaque type ScalaTropy[F[_], Result] = Impl[F, Result]

  def apply[F[_]: Monad, Result](program: MultiPartyV2[F] ?=> F[Result]): ScalaTropy[F, Result] = Impl(program)

  extension [F[_], Result](trope: ScalaTropy[F, Result])
    def projectedOn[Local <: Peer: PeerTag](using
        networkRes: Set[Resource[F, Network[F, Local]]],
    )(using MonadCancel[F, Throwable]): F[Result] =
      networkRes.toList.traverse(identity).map(_.toSet).use(projection)

    def projectedOn[Local <: Peer: PeerTag](using
        network: Set[Network[F, Local]],
    )(using Monad[F]): F[Result] =
      projection[Local](network)

    // TODO: currently, not used
    // def projectedOnMultiple[Local <: Peer: PeerTag](using
    //     networks: List[Network[F, Local]],
    // )(using Monad[F], Parallel[F]): F[List[Result]] =
    //   networks.parTraverse(projection)

    private inline def projection[Local <: Peer: PeerTag](networks: Set[Network[F, Local]])(using Monad[F]): F[Result] =
      val env = Environment.make[F]
      given MultiPartyV2[F] = MultiPartyV2.make(env, networks)
      trope.program

  private class Impl[F[_]: Monad, Result](val program: MultiPartyV2[F] ?=> F[Result])
