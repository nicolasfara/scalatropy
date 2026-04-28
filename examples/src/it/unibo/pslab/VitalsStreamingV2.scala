package it.unibo.pslab

import it.unibo.pslab.ScalaTropyV2.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiPartyV2
import it.unibo.pslab.multiparty.MultiPartyV2.*
import it.unibo.pslab.network.IoT
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.PeersV2.*

import cats.MonadThrow
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import VitalsStreamingV2.*

object VitalsStreamingV2:
  type Device <: { type Tie <: via[IoT toSingle Gatherer] }
  type Gatherer <: { type Tie <: via[IoT toMultiple Device] }

  final case class Signature(value: String) derives ReadWriter
  final case class Vitals(id: String, heartRate: String, temperature: String, motion: String) derives ReadWriter
  final case class VitalsMsg(signature: Signature, content: Vitals) derives ReadWriter

  private val validSignatures = Set("sig-a", "sig-b", "sig-c")
  private val pseudonyms = Map(
    "alice" -> "patient-1",
    "bob" -> "patient-2",
    "carol" -> "patient-3",
  )

  def vitalsStreamingProgram[F[_]: {MonadThrow, Console}](
      script: List[VitalsMsg],
  )(using MultiPartyV2[F]): F[Unit] =
    for
      vitalsOnDevice <- on[Device]:
        F.println(s"[Device] Streaming ${script.size} vitals messages").as(script)
      vitalsOnGatherer <- coAnisotropicComm[Device, Gatherer](vitalsOnDevice)
      _ <- on[Gatherer]:
        for
          batches <- takeAll(vitalsOnGatherer)
          ordered = batches.toList.sortBy(_._1.toString)
          _ <- ordered.traverse { entry =>
            val (device, messages) = entry
            val accepted = messages.collect:
              case msg if validSignatures.contains(msg.signature.value) => pseudonymise(msg.content)
            val rejected = messages.size - accepted.size
            F.println(s"[Gatherer] $device accepted=${accepted.size}, rejected=$rejected") >>
              accepted.traverse_(vitals => F.println(s"[Gatherer] $device $vitals"))
          }
        yield ()
    yield ()

  private def pseudonymise(vitals: Vitals): Vitals =
    vitals.copy(id = pseudonyms.getOrElse(vitals.id, s"anon-${vitals.id}"))

  def aliceScript: List[VitalsMsg] = List(
    VitalsMsg(Signature("sig-a"), Vitals("alice", "72", "36.6", "rest")),
    VitalsMsg(Signature("invalid"), Vitals("mallory", "120", "39.5", "run")),
  )

  def bobScript: List[VitalsMsg] = List(
    VitalsMsg(Signature("sig-b"), Vitals("bob", "68", "36.4", "walk")),
    VitalsMsg(Signature("sig-b"), Vitals("bob", "70", "36.5", "rest")),
  )

  def carolScript: List[VitalsMsg] = List(
    VitalsMsg(Signature("sig-c"), Vitals("carol", "81", "37.1", "cycle")),
    VitalsMsg(Signature("expired"), Vitals("carol", "83", "37.0", "walk")),
  )

object LauchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    List(
      VitalsGathererV2.run,
      VitalsDeviceAliceV2.run,
      VitalsDeviceBobV2.run,
      VitalsDeviceCarolV2.run,
    ).parSequence.void

object VitalsGathererV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Gatherer](Configuration(appId = "vitals-streaming-v2"))
      .use: mqttNet =>
        ScalaTropyV2(vitalsStreamingProgram[IO](Nil)).projectedOn[Gatherer]:
          tiedTo[Device] via mqttNet

object VitalsDeviceAliceV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Device](Configuration(appId = "vitals-streaming-v2"))
      .use: mqttNet =>
        ScalaTropyV2(vitalsStreamingProgram[IO](aliceScript)).projectedOn[Device]:
          tiedTo[Gatherer] via mqttNet

object VitalsDeviceBobV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Device](Configuration(appId = "vitals-streaming-v2"))
      .use: mqttNet =>
        ScalaTropyV2(vitalsStreamingProgram[IO](bobScript)).projectedOn[Device]:
          tiedTo[Gatherer] via mqttNet

object VitalsDeviceCarolV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    MqttNetwork
      .localBroker[IO, Device](Configuration(appId = "vitals-streaming-v2"))
      .use: mqttNet =>
        ScalaTropyV2(vitalsStreamingProgram[IO](carolScript)).projectedOn[Device]:
          tiedTo[Gatherer] via mqttNet
