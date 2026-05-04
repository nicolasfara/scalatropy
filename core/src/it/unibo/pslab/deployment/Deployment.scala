package it.unibo.pslab.deployment

import it.unibo.pslab.network.{ CommunicationProtocol, NetworkManager }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }
import it.unibo.pslab.peers.PeersV2.{ TiedTo, TiedWithComm }
import scala.quoted.Quotes

/**
 * Type-safe DSL entry point for defining a ScalaTropy program's deployment configuration.
 */
object Deployment:

  def tiedTo[Remote <: Peer](using
      PeerTag[Remote],
  )[F[_], Local <: TiedTo[Remote], PeerId[_ <: Peer]](using
      deployment: Scope[F, Local, PeerId],
  ): deployment.Connection[Remote] = deployment.tiedTo[Remote]

  /**
   * Deployment scope for defining connections between the local projected peer and other peer types. Each connection is
   * bound to a network protocol, as defined by the program's architectural definition.
   * @note
   *   All network protocols must encode peer identifiers using the same [[PeerId]] type constructor.
   */
  class Scope[F[_], Local <: Peer, PeerId[_ <: Peer]]:
    private var localNetworks: Set[NetworkManager[F, Local, PeerId]] = Set()

    opaque type Connection[Remote <: Peer] = PeerTag[Remote]

    /**
     * Define a connection between the local projected peer and a remote peer type which is tied to by the architectural
     * specification.
     */
    def tiedTo[Remote <: Peer](using
        remoteTag: PeerTag[Remote],
    )(using Local <:< TiedTo[Remote]): Connection[Remote] = remoteTag

    extension [Remote <: Peer](rc: Connection[Remote])

      /**
       * Binds a network protocol to the connection between the locally projected peer and the remote peer type
       * specified by [[tiedTo]], ensuring the protocol is compatible with the program's architectural definition.
       *
       * @param network
       *   the network to use for communication
       */
      infix def via[
          Protocol >: Net <: CommunicationProtocol,
          Net <: NetworkManager[F, Local, PeerId],
      ](network: Net)(using Local <:< TiedWithComm[Remote, Protocol]): Unit =
        localNetworks = localNetworks + network

    def networks: Set[NetworkManager[F, Local, PeerId]] = localNetworks

  def collectTiedPeers(using quotes: Quotes)(term: quotes.reflect.Term): Set[String] =
    import quotes.reflect.*
    var found = Set.empty[String]
    new TreeAccumulator[Unit]:
      def foldTree(acc: Unit, tree: Tree)(owner: Symbol): Unit =
        tree match
          case Inlined(_, bindings, body) => foldTree(acc, body)(owner)
          case Apply(TypeApply(Apply(TypeApply(fun, List(tpt)), _), _), _) if fun.symbol.name == "tiedTo" =>
            found += tpt.tpe.typeSymbol.name
          case elem => foldOverTree(acc, tree)(owner)
    .foldTree((), term)(Symbol.spliceOwner)
    found.toSet
