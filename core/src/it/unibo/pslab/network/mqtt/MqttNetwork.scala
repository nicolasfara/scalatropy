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

  case class Address(tag: String, clientId: String) derives ReadWriter

  object Topics:
    val presenceTopic = "pslab/presence"
    val peersBaseTopic = "pslab/peers"
    val valuesTopic = (tag: String, clientId: String) => s"$peersBaseTopic/$tag/$clientId"

  import Topics.*

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
      incomingMsgs <- Resource.eval(Ref.of(Map.empty[Address, Deferred[F, Array[Byte]]])) // TODO: add Reference
      activePeers <- Resource.eval(Ref.of(Map.empty[Address, Long]))
      msgsTopic = valuesTopic(localPeerTag.toString(), clientId)
      _ <- Resource.eval(session.subscribe(List(msgsTopic, presenceTopic).map((_, AtLeastOnce)).toVector))
      network = MqttNetworkImpl(clientId, session, incomingMsgs, activePeers)
      _ <- network.publishPresence().background
      _ <- network.handleIncomingMessage.background
    yield network

  private class MqttNetworkImpl[F[_]: {Concurrent, Temporal, Fs2Network, Console}, LP <: Peer: PeerTag as localPeerTag](
      clientId: String,
      session: Session[F],
      queues: Ref[F, Map[Address, Deferred[F, Array[Byte]]]],
      activePeers: Ref[F, Map[Address, Long]],
  ) extends Network[F, LP]:
    override type Format = Array[Byte]

    override type Address[P <: Peer] = MqttNetwork.Address

    override val localAddress: Address[LP] = Address(localPeerTag.toString(), clientId)

    val msgsTopic = valuesTopic(localPeerTag.toString(), clientId)

    private[MqttNetwork] def handleIncomingMessage: F[Unit] = session.messages
      .evalMap:
        case Message(`presenceTopic`, data) =>
          for
            now <- Temporal[F].realTime.map(_.toMillis)
            peerAddress = upickle.read[Address[?]](data.toArray)
            _ <- activePeers.update(_.updated(peerAddress, now))
          yield ()
        case Message(`msgsTopic`, data) =>
          for
            (address, payload) = upickle.read[(Address[?], Array[Byte])](data.toArray)
            d <- Deferred[F, Array[Byte]]
            existing <- queues.modify: m =>
              m.get(address) match
                case Some(found) => (m - address, found)
                case None        => (m.updated(address, d), d)
            _ <- existing.complete(payload)
          yield ()
        case _ => Concurrent[F].unit
      .compile
      .drain

    private[MqttNetwork] def publishPresence(interval: FiniteDuration = 500.millis): F[Unit] = (
      session.publish(
        presenceTopic,
        upickle.write(localAddress).getBytes().toVector,
        AtLeastOnce,
      ) >> Temporal[F].sleep(interval)
    ).foreverM

    override def alivePeersOf[RP <: Peer: PeerTag as remotePeerTag]: F[Iterable[Address[RP]]] =
      for
        now <- Temporal[F].realTime.map(_.toMillis)
        peersMap <- activePeers.get
        res = peersMap.collect:
          case (addr, lastSeen) if now - lastSeen <= 10_000 && addr.tag == remotePeerTag.toString => addr
      yield res

    override def send[V: EncodableTo[F, Format], To <: Peer: PeerTag](
        value: V,
        resource: Reference,
        to: Address[To],
    ): F[Unit] =
      for
        encodedValue <- encode(value)
        payload = upickle.write((localAddress, encodedValue)).getBytes().toVector
        _ <- session.publish(s"pslab/peers/${to.tag}/${to.clientId}", payload, AtLeastOnce)
      yield ()

    override def receive[V: DecodableFrom[F, Format], From <: Peer: PeerTag](
        resource: Reference,
        from: Address[From],
    ): F[V] =
      for
        d <- Deferred[F, Array[Byte]]
        toWaitOn <- queues.modify: m =>
          m.get(from) match
            case Some(found) => (m - from, found)
            case None        => (m.updated(from, d), d)
        data <- toWaitOn.get.flatMap(decode)
      yield data
