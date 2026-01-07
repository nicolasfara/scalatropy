package it.unibo.pslab.local

import cats.~>
import it.unibo.pslab.multiparty.MultiPartyGrammar
import it.unibo.pslab.multiparty.MultiParty.LocalPeer
import it.unibo.pslab.peers.Peers.Peer

object Local:
  def globalToLocal[P <: Peer](lp: LocalPeer[P]): MultiPartyGrammar ~> LocalAlgebra = new (MultiPartyGrammar ~> LocalAlgebra):
    def apply[T](fa: MultiPartyGrammar[T]): LocalAlgebra[T] = fa match
      case MultiPartyGrammar.On(ps, body) => ???
      case MultiPartyGrammar.CommPerPeer(ps, placed) => ???
      case MultiPartyGrammar.Comm(ps, b) => ???
      case MultiPartyGrammar.AsLocal(ps, placed) => ???
      case MultiPartyGrammar.AsLocalAll(ps, placed) => ???
      case MultiPartyGrammar.ForEachPeer(ps, body) => ???
      case MultiPartyGrammar.Remotes(ps) => ???
