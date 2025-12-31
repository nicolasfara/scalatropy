package it.unibo.pslab.multiparty

import it.unibo.pslab.peers.Peers.Peer

object MultiParty:

  trait PeerScope[P <: Peer]
  trait Many[+V]
  trait PerPeer[+V]
  trait Remote[-P <: Peer]

  trait LocalPeer[P <: Peer]
