package it.unibo.pslab.multiparty

import scala.annotation.nowarn
import it.unibo.pslab.peers.Peers.*
import cats.free.Free
import it.unibo.pslab.multiparty.MultiParty.Many
import it.unibo.pslab.multiparty.MultiParty.LocalPeer
import it.unibo.pslab.multiparty.MultiParty.PeerScope
import it.unibo.pslab.multiparty.MultiParty.PerPeer
import it.unibo.pslab.multiparty.MultiParty.Remote

object MultiPartyLanguage:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P]

  private enum Placement[+V, -P <: Peer]:
    case Local(ref: String, value: V) extends Placement[V, P]
    case Remote(ref: String) extends Placement[V, P]

  type PlacedKind[From <: Peer, To <: Peer, V] = To match
    case TieTo.TieToSingle[From]   => V on To
    case TieTo.TieToMultiple[From] => Many[V] on To

  type MultiParty[T] = Free[MultiPartyGrammar, T]

  @nowarn
  inline def on[P <: Peer](using lp: LocalPeer[?])[V](body: PeerScope[P] ?=> MultiParty[V] | V): MultiParty[V on P] =
    given ps: PeerScope[P] = new PeerScope[P] {}
    Free.liftF(MultiPartyGrammar.On[V, P](lp, _ => body))

  inline def comm[From <: TieTo[To], To <: TieTo[From]](using
      lp: LocalPeer[?]
  )[V](value: V on From): MultiParty[PlacedKind[From, To, V]] =
    Free.liftF(MultiPartyGrammar.Comm[V, From, To](lp, value))

  inline def commPerPeer[From <: TieTo.TieToMultiple[To], To <: TieTo.TieToSingle[From]](using
      lp: LocalPeer[?]
  )[V](value: PerPeer[V] on From): MultiParty[V on To] =
    Free.liftF(MultiPartyGrammar.CommPerPeer[V, From, To](lp, value))

  inline def forEachPeer[From <: TieTo.TieToMultiple[To], To <: Peer](using
      lp: LocalPeer[?]
  )[V](body: PartialFunction[Remote[To], V]): MultiParty[PerPeer[V]] =
    Free.liftF(MultiPartyGrammar.ForEachPeer[V, From, To](lp, body))

  inline def asLocal[P <: Peer: PeerScope](using lp: LocalPeer[?])[V](placed: V on P): MultiParty[V] =
    Free.liftF(MultiPartyGrammar.AsLocal[V, P](lp, placed))

  inline def asLocalAll[P <: Peer: PeerScope](using
      lp: LocalPeer[?]
  )[V](placed: Many[V] on P): MultiParty[Map[Remote[?], V]] =
    Free.liftF(MultiPartyGrammar.AsLocalAll[V, P](lp, placed))

  inline def remotes[RP <: Peer](using lp: LocalPeer[?])[L <: Peer: PeerScope]: MultiParty[Iterable[Remote[RP]]] =
    Free.liftF(MultiPartyGrammar.Remotes[RP, L](lp))
