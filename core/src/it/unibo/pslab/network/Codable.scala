package it.unibo.pslab.network

trait Encodable[F[_], -From, To]:
  def encode(value: From): F[To]

object Encodable:
  def encode[F[_], From, To](value: From)(using e: Encodable[F, From, To]): F[To] = e.encode(value)

type EncodableTo[F[_], Format] = [Message] =>> Encodable[F, Message, Format]

type BinaryEncodable[F[_], Message] = Encodable[F, Message, Array[Byte]]

trait Decodable[F[_], -From, To]:
  def decode(data: From): F[To]

object Decodable:
  def decode[F[_], From, To](data: From)(using d: Decodable[F, From, To]): F[To] = d.decode(data)

type DecodableFrom[F[_], Format] = [Message] =>> Decodable[F, Format, Message]

type BinaryDecodable[F[_], Message] = Decodable[F, Array[Byte], Message]

trait Codable[F[_], Message, Format] extends Encodable[F, Message, Format] with Decodable[F, Format, Message]

object Codable:
  export Encodable.*
  export Decodable.*

type CodableFromTo[F[_], Format] = [Message] =>> Codable[F, Message, Format]

type BinaryCodable[F[_], Message] = Codable[F, Message, Array[Byte]]
