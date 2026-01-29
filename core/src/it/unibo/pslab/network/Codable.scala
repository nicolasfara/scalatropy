package it.unibo.pslab.network

trait EncodableF[F[_], -Message]:
  def encode(value: Message): F[Array[Byte]]

type Encodable[F[_]] = [Message] =>> EncodableF[F, Message]

object Encodable:
  def encode[F[_], From](value: From)(using e: EncodableF[F, From]): F[Array[Byte]] = e.encode(value)

trait DecodableF[F[_], Message]:
  def decode(data: Array[Byte]): F[Message]

type Decodable[F[_]] = [Message] =>> DecodableF[F, Message]

object Decodable:
  def decode[F[_], To](data: Array[Byte])(using d: DecodableF[F, To]): F[To] = d.decode(data)

trait CodableF[F[_], Message] extends EncodableF[F, Message] with DecodableF[F, Message]

type Codable[F[_]] = [Message] =>> CodableF[F, Message]

object Codable:
  export Encodable.*
  export Decodable.*
