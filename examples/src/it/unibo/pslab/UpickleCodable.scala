package it.unibo.pslab

import cats.Applicative
import cats.syntax.all.*
import it.unibo.pslab.network.CodableF

object UpickleCodable:
  export upickle.default as upickle
  export upickle.*

  // TODO: can throw? MonadError / MonadThrow?
  given [F[_]: Applicative, T: ReadWriter]: CodableF[F, T] with
    override inline def encode(value: T): F[Array[Byte]] = writeBinary(value).pure[F]
    override inline def decode(data: Array[Byte]): F[T] = readBinary[T](data).pure[F]
