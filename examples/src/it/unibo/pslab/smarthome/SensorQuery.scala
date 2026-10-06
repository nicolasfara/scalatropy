package it.unibo.pslab.smarthome

import it.unibo.pslab.{ log, ScalaTropy }
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.deployment.Deployment.tiedTo
import it.unibo.pslab.multiparty.{ Label, MultiParty }
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.{ MQTT, PeerId, WebSocket }
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.network.ws.WebSocketNetwork
import it.unibo.pslab.peers.Peers.{ *, given }

import cats.{ Monad, MonadThrow }
import cats.effect.{ IO, IOApp }
import cats.effect.std.{ Console, Random }
import cats.syntax.all.*
import com.comcast.ip4s.*
import upickle.default.ReadWriter

import SensorQuery.*

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
  type BedroomCeilingLight <: NightLight
  type BedsideLamp <: NightLight
  type LivingLight <: LivingAreaLight

  enum Zone derives ReadWriter:
    case Living, Night

  enum SensorKind derives ReadWriter:
    case Thermometer, Light

  final case class DeviceProfile(name: String, room: String, zone: Zone, kind: SensorKind) derives ReadWriter

  final case class DeviceReading(
      profile: DeviceProfile,
      temperatureC: Option[Double],
      humidity: Option[Double],
      illuminanceLux: Option[Double],
      motionDetected: Boolean,
      lightOn: Option[Boolean],
  ) derives ReadWriter

  final case class HomeSnapshot(readings: List[DeviceReading]) derives ReadWriter:
    def nightOccupied: Boolean = readings.exists(r => r.profile.zone == Zone.Night && r.motionDetected)
    def nightLightOn: Boolean = readings.exists(r => r.profile.zone == Zone.Night && r.lightOn.contains(true))

    def averageNightTemperature: Option[Double] =
      average(readings.collect:
        case reading if reading.profile.zone == Zone.Night => reading.temperatureC)

    def nightIlluminance: Option[Double] =
      average(readings.collect:
        case reading if reading.profile.zone == Zone.Night => reading.illuminanceLux)

    def summary: String =
      val temperature = averageNightTemperature.map(t => f"$t%.1f C").getOrElse("unknown")
      val illuminance = nightIlluminance.map(lux => f"$lux%.0f lux").getOrElse("unknown")
      s"nightOccupied=$nightOccupied, nightLightOn=$nightLightOn, nightTemperature=$temperature, nightLux=$illuminance"

  final case class DashboardReview(snapshotSummary: String, recommended: ComfortPolicy) derives ReadWriter

  final case class ComfortPolicy(
      minNightLuxWhenOccupied: Double,
      turnOffWhenEmpty: Boolean,
      quietHours: String,
  ) derives ReadWriter

  final case class NightLightCommand(on: Boolean, reason: String) derives ReadWriter
  final case class LightCommandAck(message: String) derives ReadWriter

  enum ServerPath derives ReadWriter:
    case ActuateNightLight, ReportAlreadySatisfied

  inline def nightComfort[F[_]: {MonadThrow, Random, Console}, P <: Peer: PeerTag](using MultiParty[F]): F[Unit] =
    for
      reading <- on[Device](sampleReading[P, F])
      readingsOnServer <- coAnisotropicComm[Device, Server](reading)
      snapshot <- on[Server](buildSnapshot(readingsOnServer))
      review <- on[Server]:
        for
          current <- take(snapshot)
          recommendation = recommendPolicy(current)
          _ <- log("Server built home snapshot: ")(current.summary)
          _ <- log("Server recommended comfort policy: ")(recommendation)
        yield DashboardReview(current.summary, recommendation)
      reviewOnDashboard <- comm[Server, Dashboard](review)
      policy <- on[Dashboard](approvePolicy(reviewOnDashboard))
      policyOnServer <- comm[Dashboard, Server](policy)
      command <- on[Server]:
        for
          current <- take(snapshot)
          approvedPolicy <- take(policyOnServer)
          planned = planNightLightCommand(current, approvedPolicy)
          _ <- log("Server planned night-light command: ")(planned)
        yield planned
      path <- on[Server]:
        for
          current <- take(snapshot)
          planned <- take(command)
          selected =
            if planned.on != current.nightLightOn then ServerPath.ActuateNightLight
            else ServerPath.ReportAlreadySatisfied
          _ <- log("Server selected choreography path: ")(selected)
        yield selected
      _ <- select[Server](path):
        case ServerPath.ActuateNightLight =>
          for
            commandOnNightLight <- isotropicComm[Server, NightLight](command)
            ack <- on[NightLight]:
              for
                applied <- take(commandOnNightLight)
                _ <- log("Night light applied command: ")(applied)
              yield LightCommandAck(s"Night light ${if applied.on then "on" else "off"}: ${applied.reason}")
            ackOnServer <- coAnisotropicComm[NightLight, Server](ack)
            dashboardAck <- on[Server]:
              for
                acknowledgements <- takeAll(ackOnServer)
                messages = acknowledgements.values.map(_.message).toList.sorted
                _ <- log("Server received actuator acknowledgements: ")(messages)
              yield LightCommandAck(messages.mkString("; "))
            ackOnDashboard <- comm[Server, Dashboard](dashboardAck)
            _ <- on[Dashboard]:
              take(ackOnDashboard) >>= log("Dashboard received actuation result: ")
          yield ()
        case ServerPath.ReportAlreadySatisfied =>
          for
            dashboardAck <- on[Server]:
              for
                planned <- take(command)
                ack = LightCommandAck(s"Night light unchanged: ${planned.reason}")
                _ <- log("Server skipped night-light actuation: ")(ack.message)
              yield ack
            ackOnDashboard <- comm[Server, Dashboard](dashboardAck)
            _ <- on[Dashboard]:
              take(ackOnDashboard) >>= log("Dashboard received actuation result: ")
          yield ()
    yield ()

  def buildSnapshot[F[_]: Monad](using
      lang: MultiParty[F],
      server: Label[Server],
  )(
      readingsOnServer: lang.Anisotropic[Device, DeviceReading] on Server,
  ): F[HomeSnapshot] =
    takeAll(readingsOnServer).map: readings =>
      HomeSnapshot(readings.values.toList.sortBy(_.profile.name))

  def approvePolicy[F[_]: {Monad, Console}](using
      lang: MultiParty[F],
      dashboard: Label[Dashboard],
  )(
      reviewOnDashboard: DashboardReview on Dashboard,
  ): F[ComfortPolicy] =
    for
      review <- take(reviewOnDashboard)
      _ <- log("Dashboard reviewed home snapshot: ")(review.snapshotSummary)
      approved = review.recommended.copy(minNightLuxWhenOccupied = 45.0)
      _ <- log("Dashboard approved comfort policy: ")(approved)
    yield approved

  def recommendPolicy(snapshot: HomeSnapshot): ComfortPolicy =
    val minLux = if snapshot.averageNightTemperature.exists(_ < 18.0) then 35.0 else 55.0
    ComfortPolicy(
      minNightLuxWhenOccupied = minLux,
      turnOffWhenEmpty = true,
      quietHours = "22:30-06:30",
    )

  def planNightLightCommand(snapshot: HomeSnapshot, policy: ComfortPolicy): NightLightCommand =
    val tooDark = snapshot.nightIlluminance.forall(_ < policy.minNightLuxWhenOccupied)
    if snapshot.nightOccupied && tooDark then
      NightLightCommand(on = true, s"night area occupied and below ${policy.minNightLuxWhenOccupied} lux")
    else if !snapshot.nightOccupied && policy.turnOffWhenEmpty then
      NightLightCommand(on = false, s"night area empty during ${policy.quietHours}")
    else NightLightCommand(on = snapshot.nightLightOn, "current state already satisfies the policy")

  private inline def sampleReading[P <: Peer: PeerTag, F[_]: {MonadThrow, Random}]: F[DeviceReading] =
    deviceProfile[P, F].flatMap:
      case profile @ DeviceProfile(_, _, _, SensorKind.Thermometer) =>
        for
          temperature <- between(17.0, 23.5)
          humidity <- between(38.0, 58.0)
          motion <- chance(0.15)
        yield DeviceReading(profile, Some(temperature), Some(humidity), None, motion, None)
      case profile @ DeviceProfile(_, _, _, SensorKind.Light) =>
        for
          lightOn <- chance(if profile.zone == Zone.Night then 0.35 else 0.55)
          motion <- chance(if profile.zone == Zone.Night then 0.45 else 0.30)
          ambient <- between(5.0, 120.0)
          illuminance = if lightOn then ambient + 80.0 else ambient
        yield DeviceReading(profile, None, None, Some(illuminance), motion, Some(lightOn))

  private inline def deviceProfile[P <: Peer: PeerTag, F[_]: MonadThrow]: F[DeviceProfile] =
    val peer = syntesizePeerTag[P]
    val profile =
      if peer <:< syntesizePeerTag[BedroomThermometer] then
        Some(DeviceProfile("bedroom-thermometer", "bedroom", Zone.Night, SensorKind.Thermometer))
      else if peer <:< syntesizePeerTag[KitchenThermometer] then
        Some(DeviceProfile("kitchen-thermometer", "kitchen", Zone.Living, SensorKind.Thermometer))
      else if peer <:< syntesizePeerTag[BedroomCeilingLight] then
        Some(DeviceProfile("bedroom-ceiling-light", "bedroom", Zone.Night, SensorKind.Light))
      else if peer <:< syntesizePeerTag[BedsideLamp] then
        Some(DeviceProfile("bedside-lamp", "bedroom", Zone.Night, SensorKind.Light))
      else if peer <:< syntesizePeerTag[LivingLight] then
        Some(DeviceProfile("living-light", "living room", Zone.Living, SensorKind.Light))
      else None
    profile.liftTo[F](IllegalStateException(s"Device not recognized: $peer"))

  private def between[F[_]: {Monad, Random}](min: Double, max: Double): F[Double] =
    Random[F].nextDouble.map(value => min + (value * (max - min)))

  private def chance[F[_]: {Monad, Random}](probability: Double): F[Boolean] =
    Random[F].nextDouble.map(_ < probability)

  private def average(values: List[Option[Double]]): Option[Double] =
    val flattened = values.flatten
    Option.when(flattened.nonEmpty)(flattened.sum / flattened.size)

object LauchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    Seq(
      ServerLaunch.run,
      BedroomCeilingLightLaunch.run,
      BedsideLampLaunch.run,
      BedroomThermometerLaunch.run,
      KitchenThermometerLaunch.run,
      LivingLightLaunch.run,
      DashboardLaunch.run,
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
      ScalaTropy(SensorQuery.nightComfort[IO, Server]).projectedOn[Server]:
        tiedTo[Dashboard] via ws
        tiedTo[Device] via mqtt

object BedroomThermometerLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, BedroomThermometer](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.nightComfort[IO, BedroomThermometer]).projectedOn[BedroomThermometer]:
        tiedTo[Server] via mqtt

object KitchenThermometerLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, KitchenThermometer](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.nightComfort[IO, KitchenThermometer]).projectedOn[KitchenThermometer]:
        tiedTo[Server] via mqtt

object BedroomCeilingLightLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, BedroomCeilingLight](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.nightComfort[IO, BedroomCeilingLight]).projectedOn[BedroomCeilingLight]:
        tiedTo[Server] via mqtt

object BedsideLampLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, BedsideLamp](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.nightComfort[IO, BedsideLamp]).projectedOn[BedsideLamp]:
        tiedTo[Server] via mqtt

object LivingLightLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, LivingLight](mqttConfig)
    mqttNetwork.use: mqtt =>
      ScalaTropy(SensorQuery.nightComfort[IO, LivingLight]).projectedOn[LivingLight]:
        tiedTo[Server] via mqtt

object DashboardLaunch extends IOApp.Simple:
  override def run: IO[Unit] =
    val wsNetwork = WebSocketNetwork.make[IO, Dashboard](id = "dashboard", port = port"10000", knownPeers = wsConfig)
    wsNetwork.use: ws =>
      ScalaTropy(SensorQuery.nightComfort[IO, Dashboard]).projectedOn[Dashboard]:
        tiedTo[Server] via ws
