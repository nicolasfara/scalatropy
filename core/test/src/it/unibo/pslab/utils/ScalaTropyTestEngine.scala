package it.unibo.pslab.utils

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.network.{ CodableF, Network }
import it.unibo.pslab.peers.Peers.{ extractPeerTagsLinkedTo, Peer, PeerTag }

import cats.MonadThrow
import org.scalamock.stubs.Stubs

/** A peer reference identifying peers by name, used by [[InMemoryTestNetwork]]. */
type StringPeerId[P <: Peer] = String

trait ScalaTropyTestEngine extends Stubs:

  inline def testScalaTropyProgramInIsolation[F[_]: MonadThrow, Local <: Peer](using
      PeerTag[Local],
  )[Result](
      program: MultiParty[F] ?=> F[Result],
  ): F[Result] =
    val network = stub[Network[F, Local, StringPeerId]]
    testScalaTropyProgram[F, Local](network)[Result](program)

  inline def testScalaTropyProgram[F[_]: MonadThrow, Local <: Peer](using
      PeerTag[Local],
  )(
      network: Network[F, Local, StringPeerId],
  )[Result](program: MultiParty[F] ?=> F[Result]): F[Result] =
    val connections: Map[PeerTag[?], Network[F, Local, StringPeerId]] =
      extractPeerTagsLinkedTo[Local].map(_ -> network).toMap
    ScalaTropy(program).unsafeProjectedOn[Local](connections)

  export upickle.default as upickle
  export upickle.*

  given [F[_]: MonadThrow, T: ReadWriter]: CodableF[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = F.catchNonFatal(writeBinary(value))
    override inline def decode(data: Array[Byte]): F[T] = F.catchNonFatal(readBinary[T](data))
