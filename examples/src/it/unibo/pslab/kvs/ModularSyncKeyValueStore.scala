package it.unibo.pslab.kvs

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.{ Label, MultiParty }
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
import ModularSyncKeyValueStore.{ Client, Primary, Backup }

/**
 * An equivalent version of the SyncKeyValueStore example, but with the logic of the primary and backup nodes factored
 * out into separate handlers to showcase how ScalaTropy's programs can be structured in a modular way.
 */
object ModularSyncKeyValueStore:

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

  type RequestsHandler[F[_]] =
    (lang: MultiParty[F]) ?=> lang.Anisotropic[Client, Request] on Primary => F[lang.Anisotropic[Client, Response]]

  type BackupHandler[F[_]] = MultiParty[F] ?=> List[Request.Put] on Backup => F[Unit]

  def choreo[F[_]: {Sync, Console}](using MultiParty[F]): F[Unit] =
    for
      primaryHandler <- on[Primary](primaryHandler[F])
      backupHandler <- on[Backup](backupHandler[F])
      _ <- kvs(primaryHandler, backupHandler)
    yield ()

  def kvs[F[_]: {Sync, Console}](
      primaryHandler: RequestsHandler[F] on Primary,
      backupHandler: BackupHandler[F] on Backup,
  )(using MultiParty[F]): F[Unit] =
    for
      requestOnClient <- on[Client](waitForRequest)
      requestsOnPrimary <- coAnisotropicComm[Client, Primary](requestOnClient)
      responsesOnPrimary <- on[Primary]:
        take(primaryHandler) >>= (_(requestsOnPrimary))
      putRequests <- on[Primary]:
        takeAll(requestsOnPrimary) map (_.values.collect { case r: Put => r }.toList)
      requestsOnBackups <- isotropicComm[Primary, Backup](putRequests)
      ack <- on[Backup]:
        take(backupHandler) >>= (_(requestsOnBackups))
      _ <- coAnisotropicComm[Backup, Primary](ack)
      responseOnClient <- anisotropicComm[Primary, Client](responsesOnPrimary)
      _ <- on[Client]:
        take(responseOnClient) >>= (response => F.println(s"> $response"))
      _ <- kvs(primaryHandler, backupHandler)
    yield ()

  def primaryHandler[F[_]: Sync](using Label[Primary]): F[RequestsHandler[F]] =
    inMemoryKeyValueStore[F, String, String].map: store =>
      requestsOnPrimary =>
        for
          requests <- takeAll(requestsOnPrimary)
          responses <- store processAll requests
          message <- anisotropicMessage[Primary, Client](responses, Response.Empty)
        yield message

  def backupHandler[F[_]: {Sync, Console}](using Label[Backup]): F[BackupHandler[F]] =
    inMemoryKeyValueStore[F, String, String].map: store =>
      requests =>
        for
          rqs <- take(requests)
          _ <- F.println(s"[Backup] Received ${rqs.size} requests: ${rqs.mkString(", ")}")
          _ = rqs.foreach(request => store process request)
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

object ModularSyncKeyValueStoreApp extends IOApp.Simple:
  override def run: IO[Unit] =
    List(ModularPrimaryNode.run, ModularBackupNode.run, ModularClientNode.run, ModularClientNode.run).parSequence_

object ModularPrimaryNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Primary](Configuration(appId = "modular-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(ModularSyncKeyValueStore.choreo[IO]).projectedOn[Primary]:
        tiedTo[Backup] via mqtt
        tiedTo[Client] via mqtt

object ModularBackupNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Backup](Configuration(appId = "modular-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(ModularSyncKeyValueStore.choreo[IO]).projectedOn[Backup]:
        tiedTo[Primary] via mqtt

object ModularClientNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Client](Configuration(appId = "modular-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(ModularSyncKeyValueStore.choreo[IO]).projectedOn[Client]:
        tiedTo[Primary] via mqtt
