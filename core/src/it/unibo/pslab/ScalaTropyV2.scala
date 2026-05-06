package it.unibo.pslab

import scala.compiletime.summonInline
import scala.quoted.{ Expr, Quotes, Type }

import it.unibo.pslab.deployment.Deployment
import it.unibo.pslab.multiparty.{ Environment, MultiPartyV2 }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }
import it.unibo.pslab.peers.PeersV2.extractArchitecturalLinksOf

import cats.Monad

object ScalaTropyV2:
  export Deployment.*

  opaque type ScalaTropyV2[F[_], Result, PeerId[_ <: Peer]] = Impl[F, Result, PeerId]

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
  def apply[F[_]: Monad, Result, PeerId[_ <: Peer]](
      program: MultiPartyV2[F] ?=> F[Result],
  ): ScalaTropyV2[F, Result, PeerId] = Impl(program)

  final private class Impl[F[_]: Monad, Result, PeerId[_ <: Peer]](val program: MultiPartyV2[F] ?=> F[Result])

  extension [F[_], Result, PeerId[_ <: Peer]](trope: ScalaTropyV2[F, Result, PeerId])

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
    inline def projectedOn[Local <: Peer: PeerTag](
        inline buildConnections: Deployment.Scope[F, Local, PeerId] ?=> Unit,
    )(using Monad[F]): F[Result] =
      ${ projection[F, Result, Local, PeerId]('trope, 'buildConnections) }

  private def projection[F[_]: Type, Result: Type, Local <: Peer: Type, PeerId[_ <: Peer]: Type](
      trope: Expr[ScalaTropyV2[F, Result, PeerId]],
      scopeExpr: Expr[Deployment.Scope[F, Local, PeerId] ?=> Unit],
  )(using quotes: Quotes): Expr[F[Result]] =
    import quotes.reflect.*
    val expectedPeers = extractArchitecturalLinksOf[Local].map(_._2).map(_.typeSymbol.name).toSet
    val configuredPeers = Deployment.collectTiedPeers(scopeExpr.asTerm)
    if configuredPeers.distinct.size != configuredPeers.size then
      report.errorAndAbort(
        s"""Ambiguous configuration of tied peers.
        |Configured peers: ${configuredPeers.mkString(", ")}
        |""".stripMargin,
      )
    if expectedPeers != configuredPeers.toSet then
      report.errorAndAbort(
        s"""Mismatch between expected and configured tied peers:
        |- Expected (from Local's Tie): ${expectedPeers.mkString(", ")}
        |- Configured (from deployment): ${configuredPeers.mkString(", ")}
        |""".stripMargin,
      )
    '{
      val deployment = Deployment.Scope[F, Local, PeerId]()
      $scopeExpr(using deployment)
      val env = Environment.make[F](using summonInline[Monad[F]])
      val language = MultiPartyV2.make(env, deployment.networks)(using summonInline[Monad[F]])
      $trope.program(using language)
    }
