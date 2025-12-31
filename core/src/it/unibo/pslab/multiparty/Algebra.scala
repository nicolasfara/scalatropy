package it.unibo.pslab.multiparty

import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.peers.Peers.TieTo
import it.unibo.pslab.peers.Peers.TieTo.*
import it.unibo.pslab.multiparty.MultiParty.LocalPeer
import it.unibo.pslab.multiparty.MultiParty.PeerScope
import Language.{MultiParty, on, PlacedKind}
import it.unibo.pslab.multiparty.MultiParty.PerPeer
import it.unibo.pslab.multiparty.MultiParty.Remote
import it.unibo.pslab.multiparty.MultiParty.Many

enum MultiPartyGrammar[T]:
  case On[V, P <: Peer](ps: LocalPeer[?])(value: PeerScope[P] => MultiParty[V] | V) extends MultiPartyGrammar[V on P]
  case CommPerPeer[V, From <: TieToMultiple[To], To <: TieToSingle[From]](ps: LocalPeer[?])(value: PerPeer[V] on From)
      extends MultiPartyGrammar[V on To]
  case Comm[V, From <: TieTo[To], To <: TieTo[From]](ps: LocalPeer[?])(value: V on From)
      extends MultiPartyGrammar[PlacedKind[From, To, V]]
  case AsLocal[V, P <: Peer: PeerScope](ps: LocalPeer[?])(placed: V on P) extends MultiPartyGrammar[V]
  case AsLocalAll[V, P <: Peer: PeerScope](ps: LocalPeer[?])(placed: Many[V] on P)
      extends MultiPartyGrammar[Map[Remote[?], V]]
  case ForEachPeer[V, From <: TieToMultiple[To], To <: Peer](ps: LocalPeer[?])(value: PartialFunction[Remote[To], V])
      extends MultiPartyGrammar[PerPeer[V]]
  case Remotes[RP <: Peer, L <: Peer: PeerScope](ps: LocalPeer[?]) extends MultiPartyGrammar[Iterable[Remote[RP]]]
