package it.unibo.pslab.kvs

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.{ Environment, Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.{ Applicative, Monad }
import cats.effect.{ IO, IOApp }
import cats.effect.implicits.genSpawnOps
import cats.effect.kernel.{ Async, Temporal }
import cats.effect.std.{ Console, Queue }
import cats.syntax.all.*
import upickle.default.ReadWriter

import KeyValueStore.inMemory as inMemoryKeyValueStore
import AsyncKeyValueStore.{ Client, Primary, Backup, Request }

/**
 * An asynchronous version of the Key-Value Store example, where the primary node processes client requests and
 * replicates them to the backup nodes asynchronously.
 *
 * Moreover, this example showcases how it is possible to spawn two concurrent sub-choreographies: the request
 * processing and the replication logic. They co-exist and coordinate with each other by sharing a queue of pending
 * write requests that need to be replicated: when the request processing choreography receives a write request, it puts
 * it in the queue and the replication choreography replicates them as soon as they are enqueued, without making the
 * client wait for the replication to complete before receiving a response from the primary.
 */
object AsyncKeyValueStore:

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

  def app[F[_]: {Async, Console, Temporal}](using lang: MultiParty[F]): F[Unit] =
    for
      q <- on[Primary](Queue.unbounded[F, Request.Put])
      app = Choreo[F](q)
      _ <- app.replicate(using lang.fresh).start
      _ <- app.kvs
    yield ()

  class Choreo[F[_]: {Async, Console, Temporal}](queue: Queue[F, Put] on Primary):

    def kvs(using lang: MultiParty[F]): F[Unit] =
      def loop(handler: RequestsHandler[F] on Primary): F[Unit] = for
        requestOnClient <- on[Client](waitForRequest)
        requestsOnPrimary <- coAnisotropicComm[Client, Primary](requestOnClient)
        responsesOnPrimary <- on[Primary](take(handler) >>= (_(requestsOnPrimary)))
        responseOnClient <- anisotropicComm[Primary, Client](responsesOnPrimary)
        _ <- on[Client]:
          take(responseOnClient) >>= (response => F.println(s"> $response"))
        _ <- loop(handler)
      yield ()
      on[Primary](storeHandler).flatMap(loop)

    private def storeHandler(using Label[Primary]): F[RequestsHandler[F]] =
      inMemoryKeyValueStore[F, String, String].map: store =>
        requestsOnPrimary =>
          for
            q <- take(queue)
            requests <- takeAll(requestsOnPrimary)
            _ <- requests.collect { case (_, put: Put) => q.offer(put) }.toList.sequence
            responses <- store processAll requests
            message <- anisotropicMessage[Primary, Client](responses, Response.Empty)
          yield message

    def replicate(using MultiParty[F]): F[Unit] =
      def loop(backupHandler: BackupHandler[F] on Backup): F[Unit] =
        for
          requestsToReplicate <- on[Primary]:
            take(queue) >>= (_.tryTakeN(None))
          requestsOnBackup <- isotropicComm[Primary, Backup](requestsToReplicate)
          ack <- on[Backup](take(backupHandler) >>= (_(requestsOnBackup)))
          _ <- coAnisotropicComm[Backup, Primary](ack)
          _ <- F.sleep(5.second)
          _ <- loop(backupHandler)
        yield ()
      on[Backup](backupHandler).flatMap(loop)

    private def backupHandler(using Label[Backup]): F[BackupHandler[F]] =
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

object AsyncKeyValueStoreApp extends IOApp.Simple:
  override def run: IO[Unit] =
    List(AsyncPrimaryNode.run, AsyncBackupNode.run, AsyncClient1Node.run, AsyncClient2Node.run).parSequence_

object AsyncPrimaryNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Primary](Configuration(appId = "async-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(AsyncKeyValueStore.app[IO]).projectedOn[Primary]:
        tiedTo[Backup] via mqtt
        tiedTo[Client] via mqtt

object AsyncBackupNode extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Backup](Configuration(appId = "async-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(AsyncKeyValueStore.app[IO]).projectedOn[Backup]:
        tiedTo[Primary] via mqtt

object AsyncClient1Node extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Client](Configuration(appId = "async-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(AsyncKeyValueStore.app[IO]).projectedOn[Client]:
        tiedTo[Primary] via mqtt

object AsyncClient2Node extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Client](Configuration(appId = "async-kvs"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(AsyncKeyValueStore.app[IO]).projectedOn[Client]:
        tiedTo[Primary] via mqtt
