package it.unibo.pslab.network.mqtt

import scala.concurrent.duration.DurationLong

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{ Decodable, Encodable, Network }
import it.unibo.pslab.network.Codable.{ decode, encode }
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

import cats.data.NonEmptyList
import cats.effect.kernel.{ Concurrent, Deferred, Ref, Resource, Temporal }
import cats.effect.std.Console
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.comcast.ip4s.{ Host, Port }
import fs2.io.net.Network as Fs2Network
import net.sigusr.mqtt.api.{ Message, Session, SessionConfig, TransportConfig }
import net.sigusr.mqtt.api.QualityOfService.AtLeastOnce
import upickle.default as upickle

import upickle.ReadWriter
import java.util.UUID

object MqttNetwork:

  case class Address(tag: PeerTag[?], clientId: String) derives ReadWriter

  object Configuration:
    val startTopic = "pslab/start"
    val presenceTopic = "pslab/presence"
    val peersBaseTopic = "pslab/peers"
    val appMsgsTopic = (tag: String, clientId: String) => s"$peersBaseTopic/$tag/$clientId"
    val keepAliveInterval = 500.millis
    val aliveTimeout = 10.seconds

  import Configuration.*

  def localBroker[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String = s"pslab-${UUID.randomUUID()}",
  ): Resource[F, Network[F, LP]] = make(
    TransportConfig(Host.fromString("localhost").get, Port.fromInt(1883).get),
    SessionConfig(clientId, cleanSession = false),
  )

  def make[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      transportConfig: TransportConfig[F],
      sessionConfig: SessionConfig,
  ): Resource[F, Network[F, LP]] =
    for
      _ <- Resource.eval(F.println(s"=== Peers startup configuration ==="))
      session <- Session(transportConfig, sessionConfig)
      incomingMsgs <- Resource.eval(Ref.of(Map.empty[(Address, Reference), Deferred[F, Array[Byte]]]))
      alivePeers <- Resource.eval(Ref.of(Set.empty[Address]))
      msgsTopic = appMsgsTopic(localPeerTag.toString(), sessionConfig.clientId)
      _ <- Resource.eval(session.subscribe(List(presenceTopic, startTopic, msgsTopic).map((_, AtLeastOnce)).toVector))
      started <- Resource.eval(Deferred[F, Unit])
      network = MqttNetworkImpl(sessionConfig.clientId, session, started, incomingMsgs, alivePeers)
      _ <- network.handleIncomingMessage.background
      _ <- network.publishPresenceUntilStart.background
      _ <- network.raceForStart.background
      _ <- Resource.eval(started.get)
      _ <- Resource.eval(alivePeers.get.flatMap(peers => F.println(s"=== Connected peers: ${peers} ===")))
    yield network

  private class MqttNetworkImpl[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String,
      session: Session[F],
      started: Deferred[F, Unit],
      peersMessages: Ref[F, Map[(Address, Reference), Deferred[F, Array[Byte]]]],
      peers: Ref[F, Set[Address]],
  ) extends Network[F, LP]:
    override type Address[P <: Peer] = MqttNetwork.Address

    override val localAddress: Address[LP] = Address(localPeerTag, clientId)

    val msgsTopic = appMsgsTopic(localPeerTag.toString(), clientId)

    private[MqttNetwork] def publishPresenceUntilStart: F[Unit] =
      val heartbeat = session.publish(
        presenceTopic,
        upickle.write(localAddress).getBytes().toVector,
        AtLeastOnce,
      ) >> F.sleep(keepAliveInterval)
      heartbeat.foreverM.race(started.get).void

    private[MqttNetwork] def raceForStart: F[Unit] =
      val sendGoAfterTimeout = F.sleep(aliveTimeout) >>
        session.publish(startTopic, "go".getBytes.toVector, AtLeastOnce)
      Concurrent[F].race(started.get, sendGoAfterTimeout).void

    private[MqttNetwork] def handleIncomingMessage: F[Unit] =
      session.messages
        .evalMap:
          case Message(`presenceTopic`, data)                              => onKeepAliveMsg(data)
          case Message(`msgsTopic`, data)                                  => onApplicationMsg(data)
          case Message(`startTopic`, data) if String(data.toArray) == "go" => started.complete(()).void
          case _                                                           => Concurrent[F].unit
        .compile
        .drain

    private def onKeepAliveMsg(data: Vector[Byte]): F[Unit] =
      for
        peerAddress = upickle.read[Address[?]](data.toArray)
        _ <- peers.update(_ + peerAddress)
      yield ()

    private def onApplicationMsg(data: Vector[Byte]): F[Unit] =
      for
        (address, resource, payload) = upickle.read[(Address[?], Reference, Array[Byte])](data.toArray)
        d <- Deferred[F, Array[Byte]]
        existing <- takePeerMsgOrDefer((address, resource), d)
        _ <- existing.complete(payload)
      yield ()

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[NonEmptyList[Address[RP]]] =
      peers.get.flatMap: peers =>
        val filtered = peers.filter(_.tag == remotePeerTag)
        NonEmptyList.fromList(filtered.toList) match
          case Some(nel) => nel.pure
          case None => Concurrent[F].raiseError(new NoSuchElementException(s"No alive peers of ${remotePeerTag} found"))

    override def send[V: Encodable[F], To <: Peer: PeerTag](value: V, resource: Reference, to: Address[To]): F[Unit] =
      for
        encodedValue <- encode(value)
        payload = upickle.write((localAddress, resource, encodedValue)).getBytes().toVector
        _ <- session.publish(appMsgsTopic(to.tag.toString(), to.clientId), payload, AtLeastOnce)
      yield ()

    override def receive[V: Decodable[F], From <: Peer: PeerTag](resource: Reference, from: Address[From]): F[V] =
      for
        d <- Deferred[F, Array[Byte]]
        toWaitOn <- takePeerMsgOrDefer((from, resource), d)
        data <- toWaitOn.get.flatMap(decode)
      yield data

    private def takePeerMsgOrDefer(key: (Address[?], Reference), d: Deferred[F, Array[Byte]]) =
      peersMessages.modify: m =>
        m.get(key) match
          case Some(found) => (m - key, found)
          case None        => (m.updated(key, d), d)
