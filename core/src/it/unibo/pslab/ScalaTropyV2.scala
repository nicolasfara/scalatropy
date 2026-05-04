package it.unibo.pslab

import it.unibo.pslab.deployment.Deployment
import it.unibo.pslab.multiparty.{ Environment, MultiPartyV2 }
import it.unibo.pslab.network.NetworkManager
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.Monad
import scala.quoted.Expr
import scala.quoted.Quotes
import scala.compiletime.summonInline
import scala.quoted.Type
import it.unibo.pslab.peers.PeersV2.Quantifier

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
    inline def projectedOnV2[Local <: Peer: PeerTag](
        inline buildConnections: Deployment.Scope[F, Local, PeerId] ?=> Unit,
    )(using Monad[F]): F[Result] =
      ${ projectionCheck[F, Result, Local, PeerId]('trope, 'buildConnections) }

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
        buildConnections: Deployment.Scope[F, Local, PeerId] ?=> Unit,
    )(using Monad[F]): F[Result] =
      val deployment = Deployment.Scope[F, Local, PeerId]()
      buildConnections(using deployment)
      projection[Local](deployment.networks)

    private inline def projection[Local <: Peer: PeerTag](
        networks: Set[NetworkManager[F, Local, PeerId]],
    )(using Monad[F]): F[Result] =
      val env = Environment.make[F]
      val language = MultiPartyV2.make(env, networks)
      trope.program(using language)

  private def projectionCheck[F[_]: Type, Result: Type, Local <: Peer: Type, PeerId[_ <: Peer]: Type](
      trope: Expr[ScalaTropyV2[F, Result, PeerId]],
      scopeExpr: Expr[Deployment.Scope[F, Local, PeerId] ?=> Unit],
  )(using quotes: Quotes): Expr[F[Result]] =
    import quotes.reflect.*

    def flattenAnd(t: TypeRepr): List[TypeRepr] = t match
      case AndType(l, r) => flattenAnd(l) ++ flattenAnd(r)
      case t             => List(t)

    def extractTies(tpe: TypeRepr): List[TypeRepr] =
      tpe.typeSymbol.info match
        //         tpe lower bound
        //              v   refinement parent type   Tie lower bound
        //              v             v                    v
        case TypeBounds(_, Refinement(_, "Tie", TypeBounds(_, upperBound))) =>
          flattenAnd(upperBound)
        case other =>
          report.errorAndAbort(s"Expected type X <: { type Tie <: ... }, got: ${other.show}")

    def extractCommAndPeer(tpe: TypeRepr): (TypeRepr, TypeRepr) =
      if !(tpe <:< TypeRepr.of[Quantifier[?, ?]]) then
        report.errorAndAbort(s"Expected a Quantifier type, got: ${tpe.show}")
      tpe.typeArgs match
        // `AppliedType` represents the application of a type constructor, in this case `Quantifier[Comm, Peer]`
        case appliedType :: Nil =>
          appliedType.typeArgs match
            case comm :: peer :: Nil => (comm, peer)
            case _                   => report.errorAndAbort("Expected a type constructor Quantifier[Comm, Peer].")
        case _ => report.errorAndAbort("Cannot extract communication protocol.")

    val expectedPeers = extractTies(TypeRepr.of[Local]).map(extractCommAndPeer).map(_._2).map(_.typeSymbol.name).toSet
    val configuredPeers = Deployment.collectTiedPeers(scopeExpr.asTerm)
    if expectedPeers != configuredPeers then
      report.errorAndAbort(
        s"""Mismatch between expected and configured tied peers!
        |Expected (from Local's Tie): ${expectedPeers.mkString(", ")}
        |Configured (from deployment): ${configuredPeers.mkString(", ")}
        |""".stripMargin,
      )
    '{
      val deployment = Deployment.Scope[F, Local, PeerId]()
      $scopeExpr(using deployment)
      val env = Environment.make[F](using summonInline[Monad[F]])
      val language = MultiPartyV2.make(env, deployment.networks)(using summonInline[Monad[F]])
      $trope.program(using language)
    }
