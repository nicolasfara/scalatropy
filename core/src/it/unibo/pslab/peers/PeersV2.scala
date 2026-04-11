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

  // Typed DSL for expressing architecture:
  // ```scala
  // via[MQTT toSingle Client] & via[Memory toMultiple Database]
  // ```
  infix type toSingle[Comm <: CommunicationProtocol, P <: Peer] = SingleLink[P, Comm]
  infix type toMultiple[Comm <: CommunicationProtocol, P <: Peer] = MultipleLink[P, Comm]
  type via[Q <: Quantifier[?, ?]] = Q

  // At the moment, let's stick with a single communication protocol (the Sync one)
  // later we can create a more advanced CommunicationProtocol comprising Sync and Async protocols.
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

    def extractCommAndPeer(tpe: TypeRepr): (TypeRepr, TypeRepr) =
      if !(tpe <:< TypeRepr.of[Quantifier[?, ?]]) then
        report.errorAndAbort(s"Expected a Quantifier type, got: ${tpe.show}")
      tpe.typeArgs match
        // AppliedType represents the application of a type constructor, in this case `via[Comm, Peer]`
        case appliedType :: Nil =>
          appliedType.typeArgs match
            case comm :: peer :: Nil => (comm, peer)
            case _ => report.errorAndAbort(s"Expected either toSingle[Comm, Peer] or toMultiple[Comm, Peer].")
        case _ => report.errorAndAbort(s"Cannot extract communication protocol.")

    val pComms = extractTies(TypeRepr.of[P])
      .map(extractCommAndPeer)
      .filter((_, r) => r =:= TypeRepr.of[R]) // take only the type repr of the "other" peer
      .map(_._1)
    val rComms = extractTies(TypeRepr.of[R])
      .map(extractCommAndPeer)
      .filter((_, r) => r =:= TypeRepr.of[P]) // take only the type repr of the "other" peer
      .map(_._1)

    if pComms.length > 1 || rComms.length > 1 then
      report.errorAndAbort("No more than one link can exists between two peer types")
    else if pComms.head != rComms.head then report.errorAndAbort(s"""Incompatible types:
      | - ${TypeRepr.of[P].show} has Tie = ${pComms.map(_.show).mkString(" & ")}
      | but
      | - ${TypeRepr.of[R].show} has Tie = ${rComms.map(_.show).mkString(" & ")}
      | no common communication protocol found!
      """.stripMargin)

    val tag = Expr(pComms.head.show)
    '{ CommProtocolEv[P, R]($tag) }
