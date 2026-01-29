package it.unibo.pslab.multiparty

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.peers.Peers.PeerTag

import cats.Monad
import upickle.default as upickle

import upickle.{ ReadWriter, readwriter }

trait Environment[F[_]]:
  def provide(peerTag: PeerTag[?]): F[Reference]

object Environment:
  sealed trait Reference

  private case class ReferenceImpl(id: Int, peerTag: PeerTag[?]) extends Reference

  def make[F[_]: Monad]: Environment[F] = new Environment[F]:
    private var counter = 0
    def provide(peerTag: PeerTag[?]): F[Reference] = Monad[F].pure:
      counter += 1
      ReferenceImpl(counter, peerTag)

  given ReadWriter[Reference] = readwriter[(Int, PeerTag[?])].bimap[Reference](
    { case ReferenceImpl(id, peerTag) => (id, peerTag) },
    ReferenceImpl.apply,
  )
