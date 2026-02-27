package it.unibo.pslab.network.mqtt

import java.util.UUID

import scala.concurrent.duration.{ DurationLong, FiniteDuration }
import scala.util.control.NoStackTrace

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{ Decodable, Encodable, Network, NetworkMonitor }
import it.unibo.pslab.network.Codable.{ decode, encode }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.{ NonEmptyList, OptionT }
import cats.effect.kernel.{ Concurrent, Deferred, Ref, Resource, Temporal }
import cats.effect.std.{ Console, Env }
import cats.effect.syntax.all.*
import cats.effect.implicits.parallelForGenSpawn
import cats.syntax.all.*
import com.comcast.ip4s.{ host, port, Host, Port }
import fs2.io.net.Network as Fs2Network
import net.sigusr.mqtt.api.{ Message, Session, SessionConfig, TransportConfig }
import net.sigusr.mqtt.api.QualityOfService.AtLeastOnce
import upickle.default as upickle

import upickle.ReadWriter

object MqttNetwork:

  case class Address(tag: PeerTag[?], clientId: String) derives ReadWriter

  case class Configuration(
      appId: String,
      clientId: String = s"pslab-${UUID.randomUUID()}",
      initialWaitWindow: FiniteDuration = 7.seconds,
      keepAliveInterval: FiniteDuration = 250.millis,
  )

  private object Topics:
    val start = (appId: String) => s"pslab/$appId/start"
    val presence = (appId: String) => s"pslab/$appId/presence"
    val inMsgs = (appId: String, tag: PeerTag[?], clientId: String) => s"pslab/$appId/peers/${tag.toString}/$clientId"

  sealed trait NetworkError extends NoStackTrace
  case class InvalidConfiguration(message: String) extends NetworkError
  case class NoSuchPeers(tag: PeerTag[?]) extends NetworkError:
    override def getMessage: String = s"No alive peers of type $tag found"

  def fromEnv[F[_]: {Concurrent, Env, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag](
      config: Configuration,
  ): Resource[F, Network[F, LP]] =
    def env[A](name: String, parse: String => Option[A]): F[A] =
      OptionT(F.get(name))
        .subflatMap(parse)
        .getOrElseF(Concurrent[F].raiseError(InvalidConfiguration(s"Env $name is required and must be valid")))
    for
      (host, port) <- Resource.eval:
        (env("MQTT_BROKER_HOST", Host.fromString), env("MQTT_BROKER_PORT", Port.fromString)).parTupled
      network <- make(config, TransportConfig(host, port), SessionConfig(config.clientId, cleanSession = true))
    yield network

  def localBroker[F[_]: {Concurrent, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag](
      config: Configuration,
  ): Resource[F, Network[F, LP]] =
    make(config, TransportConfig(host"localhost", port"1883"), SessionConfig(config.clientId, cleanSession = false))

  def make[F[_]: {Concurrent, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag as localPeerTag](
      networkConfig: Configuration,
      transportConfig: TransportConfig[F],
      sessionConfig: SessionConfig,
  ): Resource[F, Network[F, LP]] =
    for
      _ <- Resource.eval(F.println(s"=== Peers startup configuration [wait ${networkConfig.initialWaitWindow}] ==="))
      session <- Session(transportConfig, sessionConfig)
      incomingMsgs <- Resource.eval(Ref.of(Map.empty[(Address, Reference), Deferred[F, Array[Byte]]]))
      alivePeers <- Resource.eval(Ref.of(Set.empty[Address]))
      started <- Resource.eval(Deferred[F, Unit])
      network = MqttNetworkImpl(networkConfig, session, started, incomingMsgs, alivePeers)
      _ <- Resource.eval(network.subscribe)
      _ <- network.handleIncomingMessage.background
      _ <- network.publishPresenceUntilStart.background
      _ <- network.waitForStart.background
      _ <- Resource.eval(started.get)
      _ <- Resource.eval(alivePeers.get.flatMap(peers => F.println(s"=== Joined peers: ${peers.mkString(",")} ===")))
    yield network

  private class MqttNetworkImpl[
      F[_]: {Concurrent, Temporal, Fs2Network, Console, NetworkMonitor},
      LP <: Peer: PeerTag as localPeerTag,
  ](
      networkConfig: Configuration,
      session: Session[F],
      started: Deferred[F, Unit],
      peersMessages: Ref[F, Map[(Address, Reference), Deferred[F, Array[Byte]]]],
      peers: Ref[F, Set[Address]],
  ) extends Network[F, LP]:
    override type Address[P <: Peer] = MqttNetwork.Address

    val startTopic = Topics.start(networkConfig.appId)
    val presenceTopic = Topics.presence(networkConfig.appId)
    val inMsgsTopic = Topics.inMsgs(networkConfig.appId, localPeerTag, networkConfig.clientId)
    val startToken = "start"

    override val localAddress: Address[LP] = Address(localPeerTag, networkConfig.clientId)

    def publishPresenceUntilStart: F[Unit] =
      val heartbeat = session.publish(presenceTopic, upickle.writeBinary(localAddress), AtLeastOnce)
      (heartbeat >> F.sleep(networkConfig.keepAliveInterval)).foreverM.race(started.get).void

    def waitForStart: F[Unit] =
      val sendGoAfterTimeout = F.sleep(networkConfig.initialWaitWindow) >>
        session.publish(startTopic, upickle.writeBinary(startToken), AtLeastOnce)
      Concurrent[F].race(started.get, sendGoAfterTimeout).void

    def subscribe: F[Unit] =
      session.subscribe(List(presenceTopic, startTopic, inMsgsTopic).map((_, AtLeastOnce)).toVector).void

    def handleIncomingMessage: F[Unit] = session.messages
      .evalMap:
        case Message(`presenceTopic`, data)                   => onKeepAliveMsg(data)
        case Message(`inMsgsTopic`, data)                     => onApplicationMsg(data)
        case Message(`startTopic`, data) if data.isStartToken => started.complete(()).void
        case _                                                => F.unit
      .compile
      .drain

    extension (data: Vector[Byte]) def isStartToken: Boolean = upickle.readBinary[String](data.toArray) == startToken

    def onKeepAliveMsg(data: Array[Byte]): F[Unit] = peers.update(_ + upickle.readBinary[Address[?]](data))

    def onApplicationMsg(data: Array[Byte]): F[Unit] =
      for
        (address, resource, payload) = upickle.readBinary[(Address[?], Reference, Array[Byte])](data)
        existing <- takePeerMsgOrDefer((address, resource))
        _ <- existing.complete(payload)
      yield ()

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[NonEmptyList[Address[RP]]] =
      peers.get.flatMap: peers =>
        val filtered = peers.filter(_.tag == remotePeerTag)
        NonEmptyList.fromList(filtered.toList) match
          case Some(nel) => nel.pure
          case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeerTag))

    override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit] =
      for
        encodedValue <- encode(value).flatTap(F.onSend)
        payload = upickle.writeBinary((localAddress, resource, encodedValue))
        _ <- session.publish(Topics.inMsgs(networkConfig.appId, to.tag, to.clientId), payload, AtLeastOnce)
      yield ()

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V] =
      for
        toWaitOn <- takePeerMsgOrDefer((from, resource))
        data <- toWaitOn.get.flatTap(F.onReceive).flatMap(decode)
      yield data

    private def takePeerMsgOrDefer(key: (Address[?], Reference)) =
      for
        d <- Deferred[F, Array[Byte]]
        res <- peersMessages.modify: m =>
          m.get(key) match
            case Some(found) => (m - key, found)
            case None        => (m.updated(key, d), d)
      yield res

  private given Conversion[Array[Byte], Vector[Byte]] = _.toVector
  private given Conversion[Vector[Byte], Array[Byte]] = _.toArray
