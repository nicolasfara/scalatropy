package it.unibo.pslab.multiparty

import cats.free.Free
import it.unibo.pslab.peers.Peers.{Peer, TieToMultiple, TieToSingle}

import scala.annotation.nowarn

object MultiParty:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  infix opaque type multiOn[+V, -P <: Peer] = Placement[V, P]

  trait Token[V, P <: Peer]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  trait PeerScope[P <: Peer]

  trait Many[+V]

  trait Selected[+V]

  trait Remote[P <: Peer]

  enum MultiPartyGrammar[T]:
    case Placed[V, P <: Peer](value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
    case Par[A, PA <: Peer, B, PB <: Peer](left: PeerScope[PA] => MultiParty[A], right: PeerScope[PB] => MultiParty[B])
        extends MultiPartyGrammar[(A on PA, B on PB)]
    case Unicast[V, From <: TieToSingle[To], To <: TieToSingle[From]](value: V on From)
        extends MultiPartyGrammar[V on To]
    case Multicast[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: V on From)
        extends MultiPartyGrammar[V on To]
    case SelectiveMulticast[V, From <: TieToMultiple[To], To <: TieToSingle[From]](value: Selected[V] on From)
        extends MultiPartyGrammar[V on To]
    case Funnel[V, From <: TieToSingle[To], To <: TieToMultiple[From]](value: V on From)
        extends MultiPartyGrammar[Many[V] on To]
    case Await[V, P <: Peer: PeerScope](placed: (V | Many[V]) on P) extends MultiPartyGrammar[V]
    case AwaitAll[V, P <: Peer: PeerScope](placed: Many[V] on P) extends MultiPartyGrammar[Iterable[V]]
    case SelectProviders[V, From <: TieToMultiple[To], To <: Peer](value: Map[Remote[To], V])
        extends MultiPartyGrammar[Selected[V]]

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  @nowarn
  inline def placed[V, P <: Peer](inline body: PeerScope[P] ?=> MultiParty[V] | V): MultiParty[V on P] =
    given ps: PeerScope[P] = new PeerScope[P] {}
    Free.liftF(MultiPartyGrammar.Placed[V, P](_ => body))

  @nowarn
  inline def par[A, PA <: Peer, B, PB <: Peer](
      inline left: PeerScope[PA] ?=> MultiParty[A],
      inline right: PeerScope[PB] ?=> MultiParty[B]
  ): MultiParty[(A on PA, B on PB)] =
    given lps: PeerScope[PA] = new PeerScope[PA] {}
    given rps: PeerScope[PB] = new PeerScope[PB] {}
    Free.liftF(MultiPartyGrammar.Par[A, PA, B, PB](_ => left, _ => right))

  inline def unicast[V, From <: TieToSingle[To], To <: TieToSingle[From]](
      inline value: V on From
  ): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.Unicast[V, From, To](value))

  inline def multicast[V, From <: TieToMultiple[To], To <: TieToSingle[From]](
      inline value: V on From
  ): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.Multicast[V, From, To](value))

  inline def selectiveMulticast[V, From <: TieToMultiple[To], To <: TieToSingle[From]](
      inline value: Selected[V] on From
  ): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.SelectiveMulticast[V, From, To](value))

  inline def selectProviders[V, From <: TieToMultiple[To], To <: Peer](
      inline value: Map[Remote[To], V]
  ): MultiParty[Selected[V]] =
    Free.liftF(MultiPartyGrammar.SelectProviders[V, From, To](value))

  inline def funnel[V, From <: TieToSingle[To], To <: TieToMultiple[From]](
      inline value: V on From
  ): MultiParty[Many[V] on To] =
    Free.liftF(MultiPartyGrammar.Funnel[V, From, To](value))

  inline def await[V, P <: Peer: PeerScope](inline placed: (V | Many[V]) on P): MultiParty[V] =
    Free.liftF(MultiPartyGrammar.Await[V, P](placed))

  inline def awaitAll[V, P <: Peer: PeerScope](inline placed: Many[V] on P): MultiParty[Iterable[V]] =
    Free.liftF(MultiPartyGrammar.AwaitAll[V, P](placed))
