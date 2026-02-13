package it.unibo.pslab

import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.Quantifier.*

import cats.Functor
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Sync
import cats.effect.std.{ Console, MapRef }
import cats.syntax.all.*
import upickle.default.ReadWriter

import KeyValueStoreApp.*
import UpickleCodable.given

trait KeyValueStore[F[_], Key, Value]:
  def get(key: Key): F[Option[Value]]
  def put(key: Key, value: Value): F[Unit]

object KeyValueStore:
  def inMemory[F[_]: Sync, Key, Value]: F[KeyValueStore[F, Key, Value]] = MapRef
    .ofConcurrentHashMap[F, Key, Value]()
    .map: mapRef =>
      new KeyValueStore[F, Key, Value]:
        override def get(key: Key): F[Option[Value]] = mapRef(key).get
        override def put(key: Key, value: Value): F[Unit] = mapRef.setKeyValue(key, value)

object KeyValueStoreApp:

  type Client <: { type Tie <: Single[Primary] }
  type Primary <: { type Tie <: Multiple[Backup] & Single[Client] }
  type Backup <: { type Tie <: Single[Primary] }

  enum Request derives ReadWriter:
    case Get(key: String)
    case Put(key: String, value: String)

  enum Response derives ReadWriter:
    case Value(value: Option[String])
    case Ack()

  import Request.*, Response.*

  def keyValueStoreProgram[F[_]: {Sync, Console}](using MultiParty[F]): F[Unit] =
    for
      primaryStorage <- on[Primary](KeyValueStore.inMemory[F, String, String])
      _ <- kvs(primaryStorage)
    yield ()

  def kvs[F[_]: {Sync, Console}](
      storage: KeyValueStore[F, String, String] on Primary,
  )(using MultiParty[F]): F[Unit] =
    for
      requestOnClient <- on[Client]:
        F.println("Enter a request ([get|put] <key>):") >> F.readLine.map(parse)
      requestOnPrimary <- comm[Client, Primary](requestOnClient)
      responseOnPrimary <- on[Primary]:
        for
          storage <- take(storage)
          req <- take(requestOnPrimary)
          res <- req.handle(storage)
          _ <- F.println(s"Primary received request: $req, computed response: $res")
        yield res
      _ <- replicate(requestOnPrimary)
      responseOnClient <- comm[Primary, Client](responseOnPrimary)
      _ <- on[Client]:
        take(responseOnClient).flatMap(r => F.println(s"Client received response: $r"))
      _ <- kvs(storage)
    yield ()

  def replicate[F[_]: {Sync, Console}](request: Request on Primary)(using MultiParty[F]): F[Unit] =
    for
      requestOnBackup <- isotropicComm[Primary, Backup](request)
      ack <- on[Backup]:
        for
          request <- take(requestOnBackup)
          _ <- F.println(s"Backup received request for replication: $request")
        yield Ack()
      _ <- coAnisotropicComm[Backup, Primary](ack)
    yield ()

  extension (req: String)
    def parse: Request =
      val parts = req.split(" ", 3)
      parts(0).toLowerCase match
        case "get" => Request.Get(parts(1))
        case "put" => Request.Put(parts(1), parts(2))
        case _     => throw new IllegalArgumentException(s"Invalid request: $req")

  extension (req: Request)
    def handle[F[_]: Functor](store: KeyValueStore[F, String, String]): F[Response] =
      req match
        case Request.Get(key)        => store.get(key).map(Response.Value(_))
        case Request.Put(key, value) => store.put(key, value).map(_ => Response.Ack())

object Primary extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Primary](Configuration(appId = "keyvaluestore"))
    ScalaTropy(keyValueStoreProgram[IO]).projectedOn[Primary](using mqttNetwork)

object Backup extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Backup](Configuration(appId = "keyvaluestore"))
    ScalaTropy(keyValueStoreProgram[IO]).projectedOn[Backup](using mqttNetwork)

object Client extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Client](Configuration(appId = "keyvaluestore"))
    ScalaTropy(keyValueStoreProgram[IO]).projectedOn[Client](using mqttNetwork)
