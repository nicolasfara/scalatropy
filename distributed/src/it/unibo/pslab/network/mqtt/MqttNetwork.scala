package it.unibo.pslab.network.mqtt

import java.util.UUID

import scala.concurrent.duration.{ DurationLong, FiniteDuration }

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{
  BaseNetwork,
  Decodable,
  Encodable,
  IoT,
  Network,
  NetworkError,
  NetworkMonitor,
  NoSuchPeers,
}
import it.unibo.pslab.network.BaseNetwork.{ IncomingMessages, PeerId }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.{ NonEmptyList, OptionT }
import cats.effect.implicits.parallelForGenSpawn
import cats.effect.kernel.{ Concurrent, Deferred, Ref, Resource, Temporal }
import cats.effect.std.{ Console, Env }
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.{ host, port, Host, Port }
import fs2.io.net.Network as Fs2Network
import net.sigusr.mqtt.api.{ Message, Session, SessionConfig, TransportConfig }
import net.sigusr.mqtt.api.QualityOfService.AtLeastOnce
import upickle.default as upickle

trait MqttNetwork[F[_], LP <: Peer] extends Network[F, LP] with IoT

object MqttNetwork:

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

  case class InvalidConfiguration(message: String) extends NetworkError(message)

  def fromEnv[F[_]: {Concurrent, Env, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag](
      config: Configuration,
  ): Resource[F, MqttNetwork[F, LP]] =
    def env[A](name: String, parse: String => Option[A]): F[A] =
      OptionT(F.get(name))
        .subflatMap(parse)
        .getOrElseF(Concurrent[F].raiseError(InvalidConfiguration(s"Env $name is required and must be valid")))
    for
      (host, port) <- Resource.eval:
        (env("MQTT_BROKER_HOST", Host.fromString), env("MQTT_BROKER_PORT", Port.fromString)).parTupled
      network <- make(config, TransportConfig(host, port), SessionConfig(config.clientId, cleanSession = false))
    yield network

  def localBroker[F[_]: {Concurrent, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag](
      config: Configuration,
  ): Resource[F, MqttNetwork[F, LP]] =
    make(config, TransportConfig(host"localhost", port"1883"), SessionConfig(config.clientId, cleanSession = false))

  def make[F[_]: {Concurrent, Temporal, Fs2Network, Console, NetworkMonitor}, LP <: Peer: PeerTag as localPeerTag](
      networkConfig: Configuration,
      transportConfig: TransportConfig[F],
      sessionConfig: SessionConfig,
  ): Resource[F, MqttNetwork[F, LP]] =
    for
      _ <- Resource.eval(F.println(s"=== Peers startup configuration [wait ${networkConfig.initialWaitWindow}] ==="))
      session <- Session(transportConfig, sessionConfig)
      incomingMsgs <- Resource.eval(Ref.of(IncomingMessages.empty[F, PeerId]))
      alivePeers <- Resource.eval(Ref.of(Set.empty[PeerId]))
      started <- Resource.eval(Deferred[F, Unit])
      network = MqttNetworkImpl(networkConfig, session, started, alivePeers, incomingMsgs)
      _ <- Resource.eval(network.subscribe)
      _ <- network.handleIncomingMessage.background
      _ <- network.publishPresenceUntilStart.background
      _ <- network.waitForStart.background
      _ <- Resource.eval(started.get)
      _ <- Resource.eval(alivePeers.get.flatMap(peers => F.println(s"=== Joined peers: ${peers.mkString(",")} ===")))
    yield network

  private class MqttNetworkImpl[
      F[_]: {Concurrent, Temporal, Fs2Network, NetworkMonitor},
      LP <: Peer: PeerTag as localPeerTag,
  ](
      networkConfig: Configuration,
      session: Session[F],
      started: Deferred[F, Unit],
      peers: Ref[F, Set[PeerId]],
      protected val incomingMsgs: Ref[F, IncomingMessages[F, PeerId]],
  ) extends MqttNetwork[F, LP]
      with BaseNetwork[F, LP]:

    val startTopic = Topics.start(networkConfig.appId)
    val presenceTopic = Topics.presence(networkConfig.appId)
    val inMsgsTopic = Topics.inMsgs(networkConfig.appId, localPeerTag, networkConfig.clientId)
    val startToken = "start"

    override val local: PeerId[LP] = PeerId(localPeerTag, networkConfig.clientId)

    def publishPresenceUntilStart: F[Unit] =
      val heartbeat = session.publish(presenceTopic, upickle.writeBinary(local), AtLeastOnce)
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

    def onKeepAliveMsg(data: Array[Byte]): F[Unit] = peers.update(_ + upickle.readBinary[PeerId[?]](data))

    def onApplicationMsg(data: Array[Byte]): F[Unit] =
      for
        (address, resource, payload) = upickle.readBinary[(PeerId[?], Reference, Array[Byte])](data)
        existing <- takePeerMsgOrDefer((address, resource))
        _ <- existing.complete(payload)
      yield ()

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[NonEmptyList[PeerId[RP]]] =
      peers.get.flatMap: peers =>
        val filtered = peers.filter(_.tag == remotePeerTag)
        NonEmptyList.fromList(filtered.toList) match
          case Some(nel) => nel.pure
          case None      => Concurrent[F].raiseError(NoSuchPeers(remotePeerTag))

    override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: PeerId[To]): F[Unit] =
      for
        encodedValue <- encodeAndTrack(value)
        payload = upickle.writeBinary((local, resource, encodedValue))
        _ <- session.publish(Topics.inMsgs(networkConfig.appId, to.tag, to.id), payload, AtLeastOnce)
      yield ()

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: PeerId[From]): F[V] =
      receiveImpl(resource, from)
  end MqttNetworkImpl

  private given Conversion[Array[Byte], Vector[Byte]] = _.toVector
  private given Conversion[Vector[Byte], Array[Byte]] = _.toArray
end MqttNetwork
