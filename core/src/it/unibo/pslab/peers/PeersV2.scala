package it.unibo.pslab.peers

import it.unibo.pslab.network.CommunicationProtocol
import scala.quoted.{ Expr, Quotes }
import scala.quoted.Type

/*
 * Motivation
 * ==========
 *
 * Let's consider a simple application compound of three peer types
 * - client
 * - primary server
 * - database
 * 
 * with the following architecture:
 *
 * - many clients are connected to a primary server instance
 * - the single server instance is connected to multiple database
 *   - multiple backups exist to guarantee fault tolerance
 * - each backup is connected with the primary
 * 
 * The communication style and protocols involved between the three parties is not the same:
 * - client and primary server usually communicates via a sync communication style (e.g., ReST)
 * - primary server and the database usually communicates via an async channel (e.g. RabbitMQ).
 * - clients or edge devices may not have the ability to use some communication protocols: architecturally, we'd like
 *   to prevent these patterns at compile-time.
 * 
 * **What are the advantages of this approach?**
 * => Enforcement of communication protocol: defining a CommunicationStyle, GRPC, for example,
 *    we're preventing at compile-time client communicates without protocols ability to do so.
 * => Switch of the communication protocol at runtime based on the architecture definition.
 * 
 * First step to showcase it: keep the choreography, do not expand to multi-tier but showcase with grpc!
 */
object PeersV2:
  import Peers.Peer

  enum Quantifier[-P <: Peer, -Comm <: CommunicationProtocol]:
    case SingleLink()
    case MultipleLink()

  import Quantifier.*

  // Typed DSL for expressing architecture
  type via[Comm <: CommunicationProtocol] = Comm
  infix type toSingle[Comm <: CommunicationProtocol, P <: Peer] = SingleLink[P, Comm]
  infix type toMultiple[Comm <: CommunicationProtocol, P <: Peer] = MultipleLink[P, Comm]

  // alternative syntax
  type through[Q <: Quantifier[?, ?]] = Q

  // At the moment, let's stick with a single communication protocol...
  // later we can create a more advanced notion of Network and CommunicationProtocol comprising
  // Sync and Async protocols.
  type TiedWithSingle[P <: Peer] = { type Tie <: SingleLink[P, ?] }
  type TiedWithMultiple[P <: Peer] = { type Tie <: MultipleLink[P, ?] }

  sealed trait CommunicationProtocolEvidence[P <: Peer, R <: Peer]
  private case class CommProtocolEv[P <: Peer, R <: Peer](tag: String) extends CommunicationProtocolEvidence[P, R]

  inline given syntesizePeerTagCommProtocolEv[P <: Peer, R <: Peer]: CommunicationProtocolEvidence[P, R] =
    ${ syntesizePeerTagCommProtocolEvImpl[P, R] }

  private def syntesizePeerTagCommProtocolEvImpl[P <: Peer: Type, R <: Peer: Type](using
      quotes: Quotes,
  ): Expr[CommunicationProtocolEvidence[P, R]] =
    import quotes.reflect.*

    def flattenAnd(t: TypeRepr): List[TypeRepr] = t match
      case AndType(l, r) => flattenAnd(l) ++ flattenAnd(r)
      case t             => List(t)

    def extractTies(tpe: TypeRepr): List[TypeRepr] =
      tpe.typeSymbol.info match
        //         tpe lower bound
        //              v
        //              v   refinement parent type   Tie lower bound
        //              v             v                    v
        case TypeBounds(_, Refinement(_, "Tie", TypeBounds(_, upperBound))) =>
          flattenAnd(upperBound)
        case other =>
          report.errorAndAbort(s"Expected type X <: { type Tie <: ... }, got: ${other.show}")

    def extractComm(tpe: TypeRepr): TypeRepr =
      if !(tpe <:< TypeRepr.of[Quantifier[?, ?]]) then
        report.errorAndAbort(s"Expected a Quantifier type, got: ${tpe.show}")
      tpe.typeArgs match
        case comm :: peer :: Nil => comm
        case appliedType :: Nil  => appliedType.typeArgs.head
        case _                   => report.errorAndAbort(s"Cannot extract communication protocol.")

    val pComms = extractTies(TypeRepr.of[P]).map(extractComm)
    val rComms = extractTies(TypeRepr.of[R]).map(extractComm)
    val common = pComms.filter(t => rComms.exists(_ =:= t))

    if common.isEmpty then report.errorAndAbort(s"""
      | Incompatible types: 
      | - ${TypeRepr.of[P].show} has Tie = ${pComms.map(_.show).mkString(" & ")}
      | but
      | - ${TypeRepr.of[R].show} has Tie = ${rComms.map(_.show).mkString(" & ")}
      | no common communication protocol found!
      """.stripMargin)
    else if common.size > 1 then report.errorAndAbort(s"""
      | Ambiguous communication protocols found between ${TypeRepr.of[P].show} and ${TypeRepr.of[R].show}:
      | ${common.map(_.show).mkString("\n - ")}
      | Please disambiguate by ensuring only one common communication protocol is shared between the two peers.
      """.stripMargin)

    val tag = Expr(common.head.show)
    '{ CommProtocolEv[P, R]($tag) }
