package it.unibo.pslab.kvs

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.{ Applicative, Monad }
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Sync
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import KeyValueStore.inMemory as inMemoryKeyValueStore
import SyncKeyValueStore.{ Client, Primary, Backup }

object SyncKeyValueStore:

  type Client <: { type Tie <: via[AnyProtocol toSingle Primary] }
  type Primary <: { type Tie <: via[AnyProtocol toMultiple Backup] & via[AnyProtocol toMultiple Client] }
  type Backup <: { type Tie <: via[AnyProtocol toSingle Primary] }

  enum Request derives ReadWriter:
    case Get(key: String)
    case Put(key: String, value: String)
    case Empty

  enum Response derives ReadWriter:
    case Value(value: Option[String])
    case Ack
    case Empty

  import Request.*

  def choreo[F[_]: {Sync, Console}](using MultiParty[F]): F[Unit] =
    for
      primaryStorage <- on[Primary](inMemoryKeyValueStore[F, String, String])
      backupStorage <- on[Backup](inMemoryKeyValueStore[F, String, String])
      _ <- kvs(primaryStorage, backupStorage)
    yield ()

  def kvs[F[_]: {Sync, Console}](
      primaryStorage: KeyValueStore[F, String, String] on Primary,
      backupStorage: KeyValueStore[F, String, String] on Backup,
  )(using MultiParty[F]): F[Unit] =
    for
      requestOnClient <- on[Client](waitForRequest)
      requestsOnPrimary <- coAnisotropicComm[Client, Primary](requestOnClient)
      responsesOnPrimary <- on[Primary]:
        for
          store <- take(primaryStorage)
          requests <- takeAll(requestsOnPrimary)
          responses <- store processAll requests
          message <- anisotropicMessage[Primary, Client](responses, Response.Empty)
        yield message
      putRequests <- on[Primary]:
        takeAll(requestsOnPrimary).map(_.values.collect { case r: Put => r })
      requestsOnBackups <- isotropicComm[Primary, Backup](putRequests)
      ack <- on[Backup]:
        for
          store <- take(backupStorage)
          requests <- take(requestsOnBackups)
          _ <- F.println(s"[Backup] Received ${requests.size} requests: ${requests.mkString(", ")}")
          _ = requests.foreach(request => store process request)
        yield Response.Ack
      _ <- coAnisotropicComm[Backup, Primary](ack)
      responseOnClient <- anisotropicComm[Primary, Client](responsesOnPrimary)
      _ <- on[Client]:
        take(responseOnClient) >>= (response => F.println(s"> $response"))
      _ <- kvs(primaryStorage, backupStorage)
    yield ()

  def waitForRequest[F[_]: {Monad, Console}]: F[Request] =
    F.println("Enter a request ([get|put] <key> [value]):") >> F.readLine
      .map(_.parse)
      .flatMap:
        case Left(error) => F.errorln(s"Invalid request: $error") >> waitForRequest
        case Right(req)  => req.pure

  extension (s: String)
    def parse: Either[String, Request] =
      s.trim.split("\\s+", 3) match
        case Array("get", key)        => Right(Request.Get(key))
        case Array("put", key, value) => Right(Request.Put(key, value))
        case _                        => Left("Use: get <key> or put <key> <value>")

  extension [F[_]: Applicative](store: KeyValueStore[F, String, String])
    infix def processAll(using lang: MultiParty[F])(requests: Map[lang.Remote[Client], Request]) =
      requests.toList
        .traverse((client, request) => (store process request).tupleLeft(client))
        .map(_.toMap)

    infix def process(req: Request): F[Response] =
      req match
        case Request.Get(key)        => store.get(key).map(Response.Value(_))
        case Request.Put(key, value) => store.put(key, value).map(_ => Response.Ack)
        case Request.Empty           => Response.Empty.pure

object SyncKeyValueStoreApp extends IOApp.Simple:
  override def run: IO[Unit] = List(PrimaryNode.run, BackupNode.run, ClientNode.run, ClientNode.run).parSequence_

object PrimaryNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Primary](Configuration(appId = "kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(SyncKeyValueStore.choreo[IO]).projectedOn[Primary]:
        tiedTo[Backup] via mqtt
        tiedTo[Client] via mqtt

object BackupNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Backup](Configuration(appId = "kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(SyncKeyValueStore.choreo[IO]).projectedOn[Backup]:
        tiedTo[Primary] via mqtt

object ClientNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Client](Configuration(appId = "kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(SyncKeyValueStore.choreo[IO]).projectedOn[Client]:
        tiedTo[Primary] via mqtt
