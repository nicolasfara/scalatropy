package it.unibo.pslab.multiparty

import it.unibo.pslab.peers.PeersV2.TiedWithSingle
import it.unibo.pslab.peers.Peers.PeerTag
import it.unibo.pslab.multiparty.MultiPartyV2.on
import it.unibo.pslab.peers.Peers.Peer
import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.PeersV2.CommunicationProtocolEvidence

trait MultiPartyV2[F[_]]:
  def comm[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolEvidence[From, To],
  )[V](value: V): F[V on To]

object MultiPartyV2:
  infix opaque type on[+V, -P <: Peer] = Placement[V, P] // same as v1

  private enum Placement[+V, -P <: Peer](val res: Reference): // same as v1
    case Local(override val res: Reference, value: V) extends Placement[V, P](res)
    case Remote(override val res: Reference) extends Placement[V, P](res)
