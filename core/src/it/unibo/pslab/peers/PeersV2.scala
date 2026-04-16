package it.unibo.pslab.peers

import scala.quoted.{ Expr, Quotes, Type }

import it.unibo.pslab.network.CommunicationProtocol

object PeersV2:
  export Peers.{ Peer, PeerTag }

  enum Quantifier[-P <: Peer, -Comm <: CommunicationProtocol]:
    case SingleLink()
    case MultipleLink()

  import Quantifier.*

  // Typed DSL for expressing architecture:
  // ```scala
  // via[MQTT toSingle Client] & via[Memory toMultiple Database]
  // ```
  type via[Q <: Quantifier[?, ?]] = Q
  infix type toSingle[Comm <: CommunicationProtocol, P <: Peer] = SingleLink[P, Comm]
  infix type toMultiple[Comm <: CommunicationProtocol, P <: Peer] = MultipleLink[P, Comm]

  // At the moment, let's stick with a single communication protocol (the Sync one)
  // later we can create a more advanced CommunicationProtocol comprising Sync and Async protocols.
  type TiedWithSingle[P <: Peer] = { type Tie <: SingleLink[P, ?] }
  type TiedWithMultiple[P <: Peer] = { type Tie <: MultipleLink[P, ?] }

  sealed trait CommunicationProtocolEvidence[P <: Peer, R <: Peer] extends (CommunicationProtocol => Boolean)
  private case class CommProtocolEv[P <: Peer, R <: Peer](
      logic: CommunicationProtocol => Boolean,
  ) extends CommunicationProtocolEvidence[P, R]:
    override def apply(protocol: CommunicationProtocol): Boolean = logic(protocol)

  inline given syntesizeCommProtocolEvidence[P <: Peer, R <: Peer]: CommunicationProtocolEvidence[P, R] =
    ${ syntesizeCommProtocolEvidenceImpl[P, R] }

  @SuppressWarnings(Array("scalafix:DisableSyntax.isInstanceOf"))
  private def syntesizeCommProtocolEvidenceImpl[P <: Peer: Type, R <: Peer: Type](using
      quotes: Quotes,
  ): Expr[CommunicationProtocolEvidence[P, R]] =
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

    def extractCommsBetween[P1 <: Peer: Type, P2 <: Peer: Type] =
      extractTies(TypeRepr.of[P1])
        .map(extractCommAndPeer)
        .filter((_, peer) => peer =:= TypeRepr.of[P2]) // consider only Communication Protocols of P1 and P2
        .map(_._1)

    val pComms = extractCommsBetween[P, R]
    val rComms = extractCommsBetween[R, P]
    if pComms.length > 1 || rComms.length > 1 then
      report.errorAndAbort("No more than one link can exists between two peer types")
    else if pComms.head != rComms.head then report.errorAndAbort(s"""Incompatible types:
      | - ${TypeRepr.of[P].show} has Tie = ${pComms.map(_.show).mkString(" & ")}
      | but
      | - ${TypeRepr.of[R].show} has Tie = ${rComms.map(_.show).mkString(" & ")}
      | no common communication protocol found!
      """.stripMargin)
    else if !(pComms.head <:< TypeRepr.of[CommunicationProtocol]) then
      report.errorAndAbort(s"Extracted type ${pComms.head.show} is not a `CommunicationProtocol`")

    pComms.head.asType match
      case '[t] => '{ CommProtocolEv[P, R](_.isInstanceOf[t]) }
