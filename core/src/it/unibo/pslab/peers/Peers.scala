package it.unibo.pslab.peers

import scala.compiletime.Erased
import scala.quoted.{ Expr, Quotes, Type }

import it.unibo.pslab.network.CommunicationProtocol

import upickle.default.{ readwriter, ReadWriter }

object Peers:
  sealed trait PeerTag[-P <: Peer]:
    def baseTypeRepr: String
    infix def <:<[R <: Peer](base: PeerTag[R]): Boolean

  final private case class PeerReprImpl[-P <: Peer](baseTypeRepr: String, supertypes: List[String]) extends PeerTag[P]:
    override def toString: String = s"'$baseTypeRepr'${supertypes.mkString(" <: '", ", ", "'")}"
    override def <:<[R <: Peer](base: PeerTag[R]): Boolean =
      baseTypeRepr == base.baseTypeRepr || supertypes.contains(base.baseTypeRepr)

  given [P <: Peer] => ReadWriter[PeerTag[P]] = readwriter[(String, List[String])].bimap(
    { case PeerReprImpl(base, superTypes) => (base, superTypes) },
    (base, superTypes) => PeerReprImpl(base, superTypes),
  )

  inline given syntesizePeerTag[P <: Peer]: PeerTag[P] = ${ peerReprImpl[P] }

  private def peerReprImpl[T <: Peer: Type](using quotes: Quotes): Expr[PeerTag[T]] =
    import quotes.reflect.*

    def collectBasesOfType(tpe: TypeRepr): List[Symbol] =
      tpe match
        case Refinement(parent, _, _) =>
          collectBasesOfType(parent)
        case AndType(left, right) =>
          collectBasesOfType(left) ++ collectBasesOfType(right)
        case _ if tpe.typeSymbol.exists =>
          collectBasesOfSymbol(tpe.typeSymbol)
        case _ =>
          List.empty

    def collectBasesOfSymbol(symbol: Symbol): List[Symbol] =
      symbol.info match
        case TypeBounds(_, hi) =>
          symbol :: collectBasesOfType(hi)
        case _ =>
          List.empty

    val types = collectBasesOfSymbol(TypeRepr.of[T].typeSymbol).map(_.fullName)
    '{ PeerReprImpl(${ Expr(types.head) }, ${ Expr(types.tail) }) }
  end peerReprImpl

  type Peer = { type Tie }

  enum Quantifier[-P <: Peer, Comm <: CommunicationProtocol]:
    case SingleLink()
    case MultipleLink()

  import Quantifier.*

  /**
   * Top-level architectural description of the communication links between peer types.
   *
   * For example, to express that a peer is tied to single `Client` via `MQTT` and to multiple `Database` via `Memory`,
   * we can write:
   * {{{
   * via[MQTT toSingle Client] & via[Memory toMultiple Database]
   * }}}
   */
  type via[Q <: Quantifier[?, ?]] = Q

  /** Encodes a single communication link between two peer types, via a specific communication protocol. */
  infix type toSingle[Comm <: CommunicationProtocol, P <: Peer] = SingleLink[P, Comm]

  /** Encodes a multiple communication link between two peer types, via a specific communication protocol. */
  infix type toMultiple[Comm <: CommunicationProtocol, P <: Peer] = MultipleLink[P, Comm]

  // At the moment, let's stick with a single communication protocol (the Sync one)
  // later we can create a more advanced CommunicationProtocol comprising Sync and Async protocols.

  type TiedTo[P <: Peer] = { type Tie <: Quantifier[P, ?] }
  type TiedWithComm[P <: Peer, Comm <: CommunicationProtocol] = { type Tie <: Quantifier[P, Comm] }

  type TiedWithSingle[P <: Peer] = { type Tie <: SingleLink[P, ?] }
  type TiedWithMultiple[P <: Peer] = { type Tie <: MultipleLink[P, ?] }

  sealed trait CommunicationProtocolCompliance[P <: Peer, R <: Peer] extends Erased

  inline given syntesizeCommProtocolEvidence[P <: Peer, R <: Peer]: CommunicationProtocolCompliance[P, R] =
    ${ syntesizeCommProtocolEvidenceImpl[P, R] }

  private def syntesizeCommProtocolEvidenceImpl[P <: Peer: Type, R <: Peer: Type](using
      quotes: Quotes,
  ): Expr[CommunicationProtocolCompliance[P, R]] =
    import quotes.reflect.*

    def extractCommsBetween[P1 <: Peer: Type, P2 <: Peer: Type] =
      val requestedPeer = TypeRepr.of[P2]
      extractArchitecturalLinksOf[P1]
        .filter((_, peer) => requestedPeer <:< peer) // links to a supertype also cover its subtypes
        .map(_._1)

    val pComms = extractCommsBetween[P, R]
    val rComms = extractCommsBetween[R, P]
    if pComms.length > 1 || rComms.length > 1 then
      report.errorAndAbort("No more than one link can exists between two peer types")
    else if !(pComms.head <:< TypeRepr.of[CommunicationProtocol]) then
      report.errorAndAbort(s"Extracted type ${pComms.head.show} is not a `CommunicationProtocol`")

    '{ new CommunicationProtocolCompliance[P, R] {} }

  private[pslab] def extractArchitecturalLinksOf[P <: Peer: Type](using
      quotes: Quotes,
  ): List[(quotes.reflect.TypeRepr, quotes.reflect.TypeRepr)] =
    import quotes.reflect.*

    def flattenAnd(t: TypeRepr): List[TypeRepr] = t match
      case AndType(l, r) => flattenAnd(l) ++ flattenAnd(r)
      case t             => List(t)

    def extractTies(tpe: TypeRepr): List[TypeRepr] =
      tpe.typeSymbol.info match
        //         tpe lower bound
        //              v   refinement parent type   Tie lower bound
        //              v             v                    v
        case TypeBounds(_, Refinement(_, "Tie", TypeBounds(_, upperBound))) => flattenAnd(upperBound)
        case TypeBounds(_, upperBound) if upperBound <:< TypeRepr.of[Peer]  => extractTies(upperBound)
        case other => report.errorAndAbort(s"Expected type X <: { type Tie <: ... }, got: ${other.show}")

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

    extractTies(TypeRepr.of[P]).map(extractCommAndPeer)
