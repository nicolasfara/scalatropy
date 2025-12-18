package it.unibo.pslab.multiparty

import cats.free.Free
import it.unibo.pslab.peers.Peers.{Peer, TieTo, TieToMultiple, TieToSingle}

import scala.annotation.nowarn

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  infix opaque type multiOn[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  trait PeerScope[P <: Peer]

  trait Many[+V]

  trait PerPeer[+V]

  trait Remote[-P <: Peer]

  type PlacedKind[From <: Peer, To <: Peer, V] = To match
    case TieToSingle[From]   => V on To
    case TieToMultiple[From] => Many[V] on To

  enum MultiPartyGrammar[T]:
    case Placed[V, P <: Peer](value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
//    case Par[A, PA <: Peer, B, PB <: Peer](left: PeerScope[PA] => MultiParty[A], right: PeerScope[PB] => MultiParty[B])
//        extends MultiPartyGrammar[(A on PA, B on PB)]
//    case Unicast[V, From <: TieToSingle[To], To <: TieToSingle[From]](value: V on From)
//        extends MultiPartyGrammar[V on To]
//    case Isotropic[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: V on From)
//        extends MultiPartyGrammar[V on To]
    case SendPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: PerPeer[V] on From)
        extends MultiPartyGrammar[V on To]
//    case Funnel[V, From <: TieToSingle[To], To <: TieToMultiple[From]](value: V on From)
//        extends MultiPartyGrammar[Many[V] on To]
    case Comm[V, From <: TieTo[To], To <: TieTo[From]](value: V on From)
        extends MultiPartyGrammar[PlacedKind[From, To, V]]
    case Await[V, P <: Peer: PeerScope](placed: V on P) extends MultiPartyGrammar[V]
    case AwaitAll[V, P <: Peer: PeerScope](placed: Many[V] on P) extends MultiPartyGrammar[Map[Remote[?], V]]
    case ForEachPeer[V, From <: TieToMultiple[To], To <: Peer](value: PartialFunction[Remote[To], V])
        extends MultiPartyGrammar[PerPeer[V]]
    case Remotes[P <: Peer]() extends MultiPartyGrammar[Iterable[Remote[P]]]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  @nowarn
  inline def placed[V, P <: Peer](inline body: PeerScope[P] ?=> MultiParty[V] | V): MultiParty[V on P] =
    given ps: PeerScope[P] = new PeerScope[P] {}
    Free.liftF(MultiPartyGrammar.Placed[V, P](_ => body))

  inline def comm[V, From <: TieTo[To], To <: TieTo[From]](
      inline value: V on From
  ): MultiParty[PlacedKind[From, To, V]] =
    Free.liftF(MultiPartyGrammar.Comm[V, From, To](value))

//  @nowarn
//  inline def par[A, PA <: Peer, B, PB <: Peer](
//      inline left: PeerScope[PA] ?=> MultiParty[A],
//      inline right: PeerScope[PB] ?=> MultiParty[B]
//  ): MultiParty[(A on PA, B on PB)] =
//    given lps: PeerScope[PA] = new PeerScope[PA] {}
//    given rps: PeerScope[PB] = new PeerScope[PB] {}
//    Free.liftF(MultiPartyGrammar.Par[A, PA, B, PB](_ => left, _ => right))

//  inline def unicast[V, From <: TieToSingle[To], To <: TieToSingle[From]](
//      inline value: V on From
//  ): MultiParty[V on To] =
//    Free.liftF(MultiPartyGrammar.Unicast[V, From, To](value))
//
//  inline def isotropic[V, From <: TieToMultiple[To], To <: TieToSingle[From]](
//      inline value: V on From
//  ): MultiParty[V on To] =
//    Free.liftF(MultiPartyGrammar.Isotropic[V, From, To](value))
//
  inline def commPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](
      inline value: PerPeer[V] on From
  ): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.SendPerPeer[V, From, To](value))

  inline def forEachPeer[V, From <: TieToMultiple[To], To <: Peer](
      inline value: PartialFunction[Remote[To], V]
  ): MultiParty[PerPeer[V]] =
    Free.liftF(MultiPartyGrammar.ForEachPeer[V, From, To](value))
//
//  inline def funnel[V, From <: TieToSingle[To], To <: TieToMultiple[From]](
//      inline value: V on From
//  ): MultiParty[Many[V] on To] =
//    Free.liftF(MultiPartyGrammar.Funnel[V, From, To](value))

  inline def await[V, P <: Peer: PeerScope](inline placed: V on P): MultiParty[V] =
    Free.liftF(MultiPartyGrammar.Await[V, P](placed))

  inline def awaitAll[V, P <: Peer: PeerScope](inline placed: Many[V] on P): MultiParty[Map[Remote[?], V]] =
    Free.liftF(MultiPartyGrammar.AwaitAll[V, P](placed))

  inline def remotes[P <: Peer](): MultiParty[Iterable[Remote[P]]] =
    Free.liftF(MultiPartyGrammar.Remotes[P]())
