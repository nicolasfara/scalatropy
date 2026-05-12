package it.unibo.pslab.deployment

import scala.quoted.{ Expr, Quotes, Type }

import it.unibo.pslab.network.{ CommunicationProtocol, Network }
import it.unibo.pslab.peers.Peers.{ extractArchitecturalLinksOf, Peer, PeerTag, TiedTo, TiedWithComm }

/**
 * Type-safe DSL entry point for defining a ScalaTropy program's deployment configuration.
 */
object Deployment:

  type Builder[F[_], Local <: Peer, PeerId[_ <: Peer]] = Deployment.Scope[F, Local, PeerId] ?=> Unit

  inline def tiedTo[Remote <: Peer](using
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
    private var localNetworks: Map[PeerTag[?], Network[F, Local, PeerId]] = Map()

    opaque type Connection[Remote <: Peer] = PeerTag[Remote]

    /**
     * Define a connection between the local projected peer and a remote peer type which is tied to by the architectural
     * specification.
     */
    def tiedTo[Remote <: Peer: PeerTag as remoteTag](using Local <:< TiedTo[Remote]): Connection[Remote] = remoteTag

    extension [Remote <: Peer: PeerTag as remoteTag](rc: Connection[Remote])

      /**
       * Binds a network protocol to the connection between the locally projected peer and the remote peer type
       * specified by [[tiedTo]], ensuring the protocol is compatible with the program's architectural definition.
       *
       * @param network
       *   the network to use for communication
       */
      infix def via[
          Protocol >: Net <: CommunicationProtocol,
          Net <: Network[F, Local, PeerId],
      ](network: Net)(using Local <:< TiedWithComm[Remote, Protocol]): Unit =
        localNetworks = localNetworks + (remoteTag -> network)

    def networks: Map[PeerTag[?], Network[F, Local, PeerId]] = localNetworks

  /**
   * Compile-time validation of the deployment configuration against the program architecture, ensuring exhaustivity of
   * connection ties and absence of duplicates.
   * @param builder
   *   the deployment configuration to validate
   */
  inline def validate[F[_], Local <: Peer, PeerId[_ <: Peer]](inline builder: Builder[F, Local, PeerId]) =
    ${ validateImpl('builder) }

  private def validateImpl[F[_], Local <: Peer: Type, PeerId[_ <: Peer]](
      builderExpr: Expr[Builder[F, Local, PeerId]],
  )(using quotes: Quotes): Expr[Unit] =
    import quotes.reflect.*
    val expectedPeers = extractArchitecturalLinksOf[Local].map(_._2).map(_.typeSymbol.fullName).toSet
    val configuredPeers = collectTiedPeers(builderExpr.asTerm)
    val dupes = configuredPeers.diff(configuredPeers.distinct)
    if dupes.nonEmpty then
      report.errorAndAbort(
        s"Each peer type may only have one connection tie, but duplicates were found: ${dupes.mkString(", ")}",
      )
    if expectedPeers != configuredPeers.toSet then
      report.errorAndAbort(
        s"""|Mismatch between expected and configured tied peers:
            |- Expected (from architecture): ${expectedPeers.mkString(", ")}
            |- Configured (from deployment): ${configuredPeers.mkString(", ")}
            |""".stripMargin,
      )
    '{ () }

  private def collectTiedPeers(using quotes: Quotes)(term: quotes.reflect.Term): List[String] =
    import quotes.reflect.*
    val found = new TreeAccumulator[List[TypeRepr]]:
      def foldTree(acc: List[TypeRepr], tree: Tree)(owner: Symbol): List[TypeRepr] =
        tree match
          case TypeApply(fun, List(tpt)) if fun.symbol.name == "tiedTo" => acc :+ tpt.tpe
          case elem                                                     => foldOverTree(acc, tree)(owner)
    .foldTree(List.empty, term)(Symbol.spliceOwner)
    found.map(_.typeSymbol.fullName)
