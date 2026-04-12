package it.unibo.pslab

// A playground where performing manual tests and having fun while developing :)

object ManualTest1:
  import it.unibo.pslab.peers.PeersV2.*
  import it.unibo.pslab.multiparty.MultiPartyV2
  import it.unibo.pslab.multiparty.MultiPartyV2.*
  import it.unibo.pslab.network.mqtt.MQTT
  import it.unibo.pslab.network.memory.Memory
  import it.unibo.pslab.network.Codable
  import cats.Monad
  import cats.syntax.all.*

  type Client <: { type Tie <: via[Memory toSingle Server] }
  type Server <: { type Tie <: via[Memory toSingle Client] & via[MQTT toMultiple Sensor] }
  type Sensor <: { type Tie <: via[MQTT toMultiple Server] }

  def example[F[_]: Monad](using MultiPartyV2[F], Codable[F][String]) =
    for
      v <- on[Client]("Hello, Server!".pure)
      _ <- comm[Client, Server](v)
    yield ()
