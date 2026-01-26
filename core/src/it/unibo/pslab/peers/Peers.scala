package it.unibo.pslab.peers

import scala.quoted.{ Expr, Quotes, Type }

import upickle.default.{ readwriter, ReadWriter }

object Peers:

  sealed trait PeerTag[-P <: Peer]

  private case class PeerTagImpl[-P <: Peer](fqn: String) extends PeerTag[P]

  given [P <: Peer] => ReadWriter[PeerTag[P]] = readwriter[String].bimap[PeerTag[P]](
    { case PeerTagImpl(fqn) => fqn },
    PeerTagImpl.apply,
  )

  inline given syntesizePeerTag[P <: Peer]: PeerTag[P] = ${ syntesizePeerTagImpl[P] }

  private def syntesizePeerTagImpl[P <: Peer: Type](using quotes: Quotes): Expr[PeerTag[P]] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[P]
    val fqn = tpe.typeSymbol.fullName
    '{ PeerTagImpl[P](${ Expr(fqn) }) }

  enum Quantifier[-P <: Peer]:
    case Single()
    case Multiple()

  type Peer = { type Tie }

  type TiedWithSingle[P <: Peer] = { type Tie <: Quantifier.Single[P] }

  type TiedWithMultiple[P <: Peer] = { type Tie <: Quantifier.Multiple[P] }

  type TieTo[P <: Peer] = { type TieTo <: Quantifier[P] }

  // sealed trait Peer

  // enum TieTo[P <: Peer] extends Peer:
  //   case TieToSingle[K <: Peer]() extends TieTo[K]
  //   case TieToMultiple[K <: Peer]() extends TieTo[K]
