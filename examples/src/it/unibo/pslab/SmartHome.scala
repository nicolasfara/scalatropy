package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.{ MQTT, PeerId, WebSocket }
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.network.ws.WebSocketNetwork
import it.unibo.pslab.peers.Peers.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.{ Console, Random }
import cats.syntax.all.*
import com.comcast.ip4s.{ ipv4, port, SocketAddress }

import SmartHome.*

object SmartHome:

  // Represents the main server hosting the backend logic
  type HA <: { type Tie <: via[MQTT toMultiple Device] & via[WebSocket toSingle Dashboard] }
  // Represents the devices in the smart home
  type Device <: { type Tie <: via[MQTT toSingle HA] }
  // Represents the user dashboard
  type Dashboard <: { type Tie <: via[WebSocket toSingle HA] }
  // A light is a refinement of a device
  type Light <: Device
  // A thermostat is a refinement of a device
  type Thermostat <: Device

  def app[F[_]: {Monad, Console, Temporal, Random}](using MultiParty[F]): F[Unit] =
    for
      ack <- on[Light](0.pure)
      _ <- coAnisotropicComm[Light, HA](ack)
      aMockValueOnHA <- on[HA](F.nextDouble)
      valueOnDashboard <- comm[HA, Dashboard](aMockValueOnHA)
      _ <- on[Dashboard]:
        take(valueOnDashboard) >>= log("Dashboard received value: ")
      _ <- F.sleep(500.millis)
      valueOnDevice <- isotropicComm[HA, Light](aMockValueOnHA)
      _ <- on[Light]:
        take(valueOnDevice) >>= log("Device received value from HA: ")
      _ <- app
    yield ()
end SmartHome

object SmartHomeApp extends IOApp.Simple:
  override def run: IO[Unit] = List(DeviceApp.run, DashboardApp.run, HAApp.run).parSequence_

def mqttConfig = Configuration(appId = "smart-home")
def wsConfig = Map(
  PeerId[HA]("HA") -> SocketAddress(ipv4"127.0.0.1", port"10001"),
  PeerId[Dashboard]("dashboard") -> SocketAddress(ipv4"127.0.0.1", port"10000"),
)

object DashboardApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val wsNetwork = WebSocketNetwork.make[IO, Dashboard](id = "dashboard", port = port"10000", wsConfig)
    wsNetwork.use: ws =>
      ScalaTropy(SmartHome.app[IO]).projectedOn[Dashboard]:
        tiedTo[HA] via ws

object HAApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, HA](mqttConfig)
    val wsNetwork = WebSocketNetwork.make[IO, HA](id = "HA", port = port"10001", knownPeers = wsConfig)
    (mqttNetwork, wsNetwork).tupled.use: (mqtt, ws) =>
      ScalaTropy(SmartHome.app[IO]).projectedOn[HA]:
        tiedTo[Dashboard] via ws
        tiedTo[Device] via mqtt

object DeviceApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Light](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SmartHome.app[IO]).projectedOn[Light]:
        tiedTo[HA] via mqtt
