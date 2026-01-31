package it.unibo.pslab

import it.unibo.pslab.network.CodableF

import cats.MonadThrow

object UpickleCodable:
  export upickle.default as upickle
  export upickle.*

  given [F[_]: MonadThrow, T: ReadWriter]: CodableF[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = F.catchNonFatal(writeBinary(value))
    override inline def decode(data: Array[Byte]): F[T] = F.catchNonFatal(readBinary[T](data))
