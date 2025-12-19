package it.unibo.pslab.multiparty

import cats.free.Free
import it.unibo.pslab.peers.Peers.{Peer, TieTo}
import it.unibo.pslab.peers.Peers.TieTo.*

import scala.annotation.nowarn

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

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
    case SendPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: PerPeer[V] on From)
        extends MultiPartyGrammar[V on To]
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

  inline def commPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](
      inline value: PerPeer[V] on From
  ): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.SendPerPeer[V, From, To](value))

  inline def forEachPeer[V, From <: TieToMultiple[To], To <: Peer](
      inline value: PartialFunction[Remote[To], V]
  ): MultiParty[PerPeer[V]] =
    Free.liftF(MultiPartyGrammar.ForEachPeer[V, From, To](value))

  inline def await[V, P <: Peer: PeerScope](inline placed: V on P): MultiParty[V] =
    Free.liftF(MultiPartyGrammar.Await[V, P](placed))

  inline def awaitAll[V, P <: Peer: PeerScope](inline placed: Many[V] on P): MultiParty[Map[Remote[?], V]] =
    Free.liftF(MultiPartyGrammar.AwaitAll[V, P](placed))

  inline def remotes[P <: Peer](): MultiParty[Iterable[Remote[P]]] =
    Free.liftF(MultiPartyGrammar.Remotes[P]())
