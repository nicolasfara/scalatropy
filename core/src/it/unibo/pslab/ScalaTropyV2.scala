package it.unibo.pslab

import it.unibo.pslab.deployment.Deployment
import it.unibo.pslab.multiparty.{ Environment, MultiPartyV2 }
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.Monad

object ScalaTropyV2:
  export Deployment.*

  opaque type ScalaTropyV2[F[_], Result] = Impl[F, Result]

  /**
   * The main entry point for ScalaTropy program instantiation and interpretation. Use this method to create a
   * ScalaTropy program, and then project it on a specific peer type, providing the necessary connections with other
   * peer types, as per the architecture of the program.
   *
   * For example:
   *
   * {{{
   * ScalaTropyV2(mainProgram[IO]).projectedOn[PeerA]:
   *   tiedTo[PeerB] via IoTNetwork
   *   tiedTo[PeerC] via wsNetwork
   *   // other connections
   * }}}
   *
   * @param program
   *   the program to instantiate.
   * @return
   *   a value that can be projected on a specific peer, given an appropriate deployment strategy.
   */
  def apply[F[_]: Monad, Result](program: MultiPartyV2[F] ?=> F[Result]): ScalaTropyV2[F, Result] = Impl(program)

  final private class Impl[F[_]: Monad, Result](val program: MultiPartyV2[F] ?=> F[Result])

  extension [F[_], Result](trope: ScalaTropyV2[F, Result])
    /**
     * Perform the End Point Projection of the ScalaTropy program on a specific peer type.
     *
     * @param Local
     *   the local peer type to project on.
     * @param buildConnections
     *   the scope in which to define the connections between the local projected peer and other peer types
     * @see
     *   [[Deployment]] for the DSL syntax to define the connections and the network protocol
     * @return
     *   an effect that, when evaluated, yields the result of the program on the local peer
     */
    def projectedOn[Local <: Peer: PeerTag](
        buildConnections: Deployment.Scope[F, Local] ?=> Unit,
    )(using Monad[F]): F[Result] =
      val deployment = Deployment.Scope[F, Local]()
      buildConnections(using deployment)
      projection[Local](deployment.networks)

    private inline def projection[Local <: Peer: PeerTag](networks: Set[Network[F, Local]])(using Monad[F]): F[Result] =
      val env = Environment.make[F]
      val language = MultiPartyV2.make(env, networks)
      trope.program(using language)
