package it.unibo.pslab.utils

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.network.Network
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.MonadThrow
import cats.syntax.all.*
import org.scalamock.stubs.Stubs
import it.unibo.pslab.network.CodableF

trait ScalaTropyTestEngine extends Stubs:

  type StringPeerId[P <: Peer] = String

  inline def testInIsolationScalaTropyProgram[F[_]: MonadThrow, Local <: Peer](using
      PeerTag[Local],
  )[Result](program: MultiParty[F] ?=> F[Result]): F[Result] =
    val stubbedNetwork = stub[Network[F, Local, StringPeerId]]
    val connections = Map.empty[PeerTag[?], Network[F, Local, StringPeerId]].withDefault(_ => stubbedNetwork)
    ScalaTropy(program).unsafeProjectedOn[Local](connections)

  export upickle.default as upickle
  export upickle.*

  given [F[_]: MonadThrow, T: ReadWriter]: CodableF[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = F.catchNonFatal(writeBinary(value))
    override inline def decode(data: Array[Byte]): F[T] = F.catchNonFatal(readBinary[T](data))
