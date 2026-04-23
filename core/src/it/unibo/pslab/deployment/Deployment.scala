package it.unibo.pslab.deployment

import it.unibo.pslab.network.{ CommunicationProtocol, Network }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }
import it.unibo.pslab.peers.PeersV2.{ TiedTo, TiedWithComm }

/**
 * Type safe DSL entry point for defining the deployment strategy of a ScalaTropy program.
 */
object Deployment:

  def tiedTo[Remote <: Peer](using
      PeerTag[Remote],
  )[F[_], Local <: TiedTo[Remote]](using
      deployment: Scope[F, Local],
  ): deployment.RawConnection[Remote] = deployment.tiedTo[Remote]

  /**
   * The Deployment Scope where to define connections between the local projected peer and other peer types, along with
   * their network protocol they intend to use to communicate, in accordance with the architectural specification of the
   * program.
   */
  class Scope[F[_], Local <: Peer]:
    private var localNetworks: Set[Network[F, Local]] = Set()

    opaque type RawConnection[Remote <: Peer] = PeerTag[Remote]

    /**
     * Define a connection between the local projected peer and a remote peer type which is tied to by the architectural
     * specification.
     */
    def tiedTo[Remote <: Peer](using
        remoteTag: PeerTag[Remote],
    )(using Local <:< TiedTo[Remote]): RawConnection[Remote] = remoteTag

    extension [Remote <: Peer](rc: RawConnection[Remote])

      /**
       * Define the network protocol to be used for the connection between the local projected peer and the remote peer
       * type specified with `tiedTo`, ensuring that the specified protocol is compatible with the architectural
       * specification of the program.
       *
       * @param network
       *   the network to use for communication
       */
      infix def via[
          Protocol >: Net <: CommunicationProtocol,
          Net <: Network[F, Local],
      ](network: Net)(using Local <:< TiedWithComm[Remote, Protocol]): Unit =
        localNetworks = localNetworks + network

    def networks: Set[Network[F, Local]] = localNetworks
