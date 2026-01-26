package it.unibo.pslab.network.mqtt

import scala.concurrent.duration.{ DurationLong, FiniteDuration }

import it.unibo.pslab.multiparty.Environment.Reference
import it.unibo.pslab.network.{ DecodableFrom, EncodableTo, Network }
import it.unibo.pslab.network.Decodable.decode
import it.unibo.pslab.network.Encodable.encode
import it.unibo.pslab.peers.Peers.{ Peer, PeerTag }

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

object MqttNetwork:

  case class Address(tag: PeerTag[?], clientId: String) derives ReadWriter

  object Configuration:
    val presenceTopic = "pslab/presence"
    val peersBaseTopic = "pslab/peers"
    val appMsgsTopic = (tag: String, clientId: String) => s"$peersBaseTopic/$tag/$clientId"
    val keepAliveInterval = 500.millis
    val aliveTimeout = 10.seconds

  import Configuration.*

  type MqttNetwork[F[_], LP <: Peer] = Network[F, LP] { type Format = Array[Byte] }

  def localBroker[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String,
  ): Resource[F, MqttNetwork[F, LP]] = make(
    clientId,
    TransportConfig(Host.fromString("localhost").get, Port.fromInt(1883).get),
    SessionConfig(clientId),
  )

  def make[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String,
      transportConfig: TransportConfig[F],
      sessionConfig: SessionConfig,
  ): Resource[F, MqttNetwork[F, LP]] =
    for
      session <- Session(transportConfig, sessionConfig)
      incomingMsgs <- Resource.eval(Ref.of(Map.empty[(Address, Reference), Deferred[F, Array[Byte]]]))
      activePeers <- Resource.eval(Ref.of(Map.empty[Address, FiniteDuration]))
      msgsTopic = appMsgsTopic(localPeerTag.toString(), clientId)
      _ <- Resource.eval(session.subscribe(List(msgsTopic, presenceTopic).map((_, AtLeastOnce)).toVector))
      network = MqttNetworkImpl(clientId, session, incomingMsgs, activePeers)
      _ <- network.publishPresence.background
      _ <- network.handleIncomingMessage.background
    yield network

  private class MqttNetworkImpl[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String,
      session: Session[F],
      peersMessages: Ref[F, Map[(Address, Reference), Deferred[F, Array[Byte]]]],
      activePeers: Ref[F, Map[Address, FiniteDuration]],
  ) extends Network[F, LP]:
    override type Format = Array[Byte]

    override type Address[P <: Peer] = MqttNetwork.Address

    override val localAddress: Address[LP] = Address(localPeerTag, clientId)

    val msgsTopic = appMsgsTopic(localPeerTag.toString(), clientId)

    private[MqttNetwork] def handleIncomingMessage: F[Unit] = session.messages
      .evalMap:
        case Message(`presenceTopic`, data) => onKeepAliveMsg(data)
        case Message(`msgsTopic`, data)     => onApplicationMsg(data)
        case _                              => Concurrent[F].unit
      .compile
      .drain

    private def onKeepAliveMsg(data: Vector[Byte]): F[Unit] =
      for
        now <- Temporal[F].realTime
        peerAddress = upickle.read[Address[?]](data.toArray)
        _ <- activePeers.update(_.updated(peerAddress, now))
      yield ()

    private def onApplicationMsg(data: Vector[Byte]): F[Unit] =
      for
        (address, resource, payload) = upickle.read[(Address[?], Reference, Array[Byte])](data.toArray)
        d <- Deferred[F, Array[Byte]]
        existing <- takePeerMsgOrDefer((address, resource), d)
        _ <- existing.complete(payload)
      yield ()

    private[MqttNetwork] def publishPresence: F[Unit] = (
      session.publish(
        presenceTopic,
        upickle.write(localAddress).getBytes().toVector,
        AtLeastOnce,
      ) >> Temporal[F].sleep(keepAliveInterval)
    ).foreverM

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[Iterable[Address[RP]]] =
      for
        now <- Temporal[F].realTime
        peers <- activePeers.get
      yield peers.collect:
        case (addr, lastSeen) if now - lastSeen <= aliveTimeout && addr.tag == remotePeerTag => addr

    override def send[V: EncodableTo[F, Format], To <: Peer: PeerTag](
        value: V,
        resource: Reference,
        to: Address[To],
    ): F[Unit] =
      for
        encodedValue <- encode(value)
        payload = upickle.write((localAddress, resource, encodedValue)).getBytes().toVector
        _ <- session.publish(appMsgsTopic(to.tag.toString(), to.clientId), payload, AtLeastOnce)
      yield ()

    override def receive[V: DecodableFrom[F, Format], From <: Peer: PeerTag](
        resource: Reference,
        from: Address[From],
    ): F[V] =
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
