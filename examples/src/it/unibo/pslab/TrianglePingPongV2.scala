package it.unibo.pslab

import scala.concurrent.duration.DurationInt

import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiPartyV2
import it.unibo.pslab.multiparty.MultiPartyV2.*
import it.unibo.pslab.network.mqtt.{ MQTT, MqttNetwork }
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.PeersV2.*

import cats.Monad
import cats.effect.{ IO, IOApp }
import cats.effect.kernel.Temporal
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import TrianglePingPongV2.*

object TrianglePingPongV2:
  type Alice <: { type Tie <: via[MQTT toSingle Bob] & via[MQTT toSingle Andromeda] }
  type Bob <: { type Tie <: via[MQTT toSingle Alice] & via[MQTT toSingle Andromeda] }
  type Andromeda <: { type Tie <: via[MQTT toSingle Bob] & via[MQTT toSingle Alice] }

  def pingPongProgram[F[_]: {Monad, Console, Temporal}](using MultiPartyV2[F]): F[Unit] = for
    initial <- on[Alice](0.pure)
    _ <- pingPong(initial)
  yield ()

  def pingPong[F[_]: {Monad, Console, Temporal}](initial: Int on Alice)(using MultiPartyV2[F]): F[Unit] = for
    _ <- on[Alice]:
      for
        v <- take(initial)
        _ <- F.println(s"Alice's value: $v")
      yield ()
    aliceSendToBob <- comm[Alice, Bob](initial)
    prepareMessageToAndromeda <- on[Bob]:
      for
        v <- take(aliceSendToBob)
        _ <- F.println(s"Bob's value: $v")
      yield v + 1
    bobSendToAndromeda <- comm[Bob, Andromeda](prepareMessageToAndromeda)
    prepareMessageToAlice <- on[Andromeda]:
      for
        v <- take(bobSendToAndromeda)
        _ <- F.println(s"Andromeda's value: $v")
      yield v + 1
    pingerSum <- comm[Andromeda, Alice](prepareMessageToAlice)
    _ <- F.sleep(1.second)
    _ <- pingPong(pingerSum)
  yield ()

object BobV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Bob](Configuration(appId = "triangle-pingpong"))
    val networks = Set(mqttNetwork)
    ScalaTropyV2(pingPongProgram[IO]).projectedOn[Bob](using networks)

object AndromedaV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Andromeda](Configuration(appId = "triangle-pingpong"))
    val networks = Set(mqttNetwork)
    ScalaTropyV2(pingPongProgram[IO]).projectedOn[Andromeda](using networks)

object AliceV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Alice](Configuration(appId = "triangle-pingpong"))
    val networks = Set(mqttNetwork)
    ScalaTropyV2(pingPongProgram[IO]).projectedOn[Alice](using networks)
