package it.unibo.pslab.smarthome

import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.network.MQTT
import it.unibo.pslab.network.WebSocket
import cats.*
import cats.syntax.all.*
import it.unibo.pslab.peers.Peers.syntesizePeerTag
import cats.effect.std.*
import it.unibo.pslab.UpickleCodable.given
import upickle.default.ReadWriter
import it.unibo.pslab.log
import cats.effect.IOApp
import cats.effect.IO
import it.unibo.pslab.smarthome.SensorQuery.Server
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.network.ws.WebSocketNetwork
import it.unibo.pslab.network.PeerId
import it.unibo.pslab.smarthome.SensorQuery.Dashboard
import com.comcast.ip4s.*
import it.unibo.pslab.ScalaTropy
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.smarthome.SensorQuery.*

object SensorQuery:
  type Server <: { type Tie <: via[MQTT toMultiple Device] & via[WebSocket toSingle Dashboard] }
  type Device <: { type Tie <: via[MQTT toSingle Server] }
  type Dashboard <: { type Tie <: via[WebSocket toSingle Server] }

  type LivingArea <: Device
  type NightArea <: Device

  type Light <: Device
  type NightLight <: NightArea
  type LivingAreaLight <: LivingArea

  type BedroomThermometer <: NightArea
  type KitchenThermometer <: LivingArea
  type BedroomLight <: NightLight
  type LivingLight <: LivingAreaLight

  final case class DeviceStatus(name: String, value: Double) derives ReadWriter
  final case class LightCommandPrompt(message: String) derives ReadWriter
  final case class LightCommand(on: Boolean) derives ReadWriter
  final case class LightCommandAck(message: String) derives ReadWriter

  private inline def getDeviceStatus[P <: Peer: PeerTag, F[_]: {MonadThrow, Random}]: F[DeviceStatus] = syntesizePeerTag[P] match
    case bedroomThermometer if bedroomThermometer <:< syntesizePeerTag[BedroomThermometer] => 
      Random[F].nextDouble.map(DeviceStatus("Bedroom Thermometer", _))
    case kitchenThermometer if kitchenThermometer <:< syntesizePeerTag[KitchenThermometer] =>
       Random[F].nextDouble.map(DeviceStatus("Kitchen Thermometer", _))
    case bedroomLight if bedroomLight <:< syntesizePeerTag[BedroomLight] =>
      Random[F].nextBoolean.map(s => DeviceStatus("Bedroom Light", if s then 1.0 else 0.0))
    case livinglight if livinglight <:< syntesizePeerTag[LivingLight] =>
      Random[F].nextBoolean.map(s => DeviceStatus("Living Light", if s then 1.0 else 0.0))
    case notRecognized => MonadThrow[F].raiseError(IllegalStateException(s"Device not recognized: ${notRecognized}"))

  inline def queryDeviceState[P <: Peer: PeerTag, F[_]: {MonadThrow, Random, Console}](using MultiParty[F]) = for
    status <- on[Device] { getDeviceStatus }
    statusOnServer <- coAnisotropicComm[Device, Server](status)
    _ <- on[Server] {
      takeAll(statusOnServer).map(_.toMap.values) >>= log("Devices status: ")
    }
    prompt <- on[Server](LightCommandPrompt("Ready for night-area light command").pure)
    promptOnDashboard <- comm[Server, Dashboard](prompt)
    command <- on[Dashboard] {
      for
        prompt <- take(promptOnDashboard)
        _ <- log("Dashboard received command prompt: ")(prompt)
        command <- LightCommand(on = true).pure
        _ <- log("Dashboard requested night-area light command: ")(command)
      yield command
    }
    commandOnServer <- comm[Dashboard, Server](command)
    actuation <- on[Server] {
      take(commandOnServer).flatTap(log("Server actuating night-area light command: "))
    }
    commandOnNightLight <- isotropicComm[Server, NightLight](actuation)
    _ <- on[NightLight] {
      take(commandOnNightLight) >>= log("Night-area light command applied: ")
    }
    ack <- on[Server](LightCommandAck("Night-area light command forwarded").pure)
    ackOnDashboard <- comm[Server, Dashboard](ack)
    _ <- on[Dashboard] {
      take(ackOnDashboard) >>= log("Dashboard received server acknowledgement: ")
    }
  yield ()

object LauchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    Seq(
      ServerLaunch.run,
      BedroomLightLaunch.run,
      BedroomThermometerLaunch.run,
      KitchenThermometerLaunch.run,
      LivingLightLaunch.run,
      DashboardLaunch.run
    ).parSequence_

def mqttConfig = Configuration(appId = "smart-home")
def wsConfig = Map(
  PeerId[Server]("server") -> SocketAddress(ipv4"127.0.0.1", port"10001"),
  PeerId[Dashboard]("dashboard") -> SocketAddress(ipv4"127.0.0.1", port"10000"),
)

object ServerLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Server](mqttConfig)
    val wsNetwork = WebSocketNetwork.make[IO, Server](id = "server", port = port"10001", knownPeers = wsConfig)
    (mqttNetwork, wsNetwork).tupled.use: (mqtt, ws) =>
      ScalaTropy(SensorQuery.queryDeviceState[Server, IO]).projectedOn[Server]:
        tiedTo[Dashboard] via ws
        tiedTo[Device] via mqtt

object BedroomThermometerLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, BedroomThermometer](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.queryDeviceState[BedroomThermometer, IO]).projectedOn[BedroomThermometer]:
        tiedTo[Server] via mqtt

object KitchenThermometerLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, KitchenThermometer](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.queryDeviceState[KitchenThermometer, IO]).projectedOn[KitchenThermometer]:
        tiedTo[Server] via mqtt

object BedroomLightLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, BedroomLight](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.queryDeviceState[BedroomLight, IO]).projectedOn[BedroomLight]:
        tiedTo[Server] via mqtt

object LivingLightLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, LivingLight](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.queryDeviceState[LivingLight, IO]).projectedOn[LivingLight]:
        tiedTo[Server] via mqtt

object DashboardLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val wsNetwork = WebSocketNetwork.make[IO, Dashboard](id = "dashboard", port = port"10000", knownPeers = wsConfig)
    wsNetwork.use: ws =>
      ScalaTropy(SensorQuery.queryDeviceState[Dashboard, IO]).projectedOn[Dashboard]:
        tiedTo[Server] via ws
