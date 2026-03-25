package it.unibo.pslab

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MultiPartyV2EdgeCases extends AnyFlatSpec with should.Matchers:

  "comm" should "guarantee to compile when architecture and communication protocol are coherent" in:
    """
    | import _root_.it.unibo.pslab.peers.PeersV2.*
    | import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    | import _root_.it.unibo.pslab.network.mqtt.MqttNetwork
    | 
    | type Client <: { type Tie <: through[MqttNetwork] toSingle Server }
    | type Server <: { type Tie <: through[MqttNetwork] toSingle Client }
    |
    | def example[F[_]](using l: MultiPartyV2[F]) =
    |   l.comm[Client, Server]("Hello, Server!")
    |   ???
    """.stripMargin should compile

  "comm" should "guarantee to **NOT** compile when architecture and communication protocol are incoherent" in:
    """
    | import _root_.it.unibo.pslab.peers.PeersV2.*
    | import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    | import _root_.it.unibo.pslab.network.mqtt.MqttNetwork
    | import _root_.it.unibo.pslab.network.memory.InMemoryNetwork
    | 
    | type Client <: { type Tie <: through[MqttNetwork] toSingle Server }
    | type Server <: { type Tie <: through[InMemoryNetwork] toSingle Client }
    |
    | def example[F[_]](using l: MultiPartyV2[F]) =
    |   l.comm[Client, Server]("Hello, Server!")
    |   ???
    """.stripMargin shouldNot typeCheck

// A playground where performing manual tests and having fun while developing!
object ManualTests:
  import it.unibo.pslab.peers.PeersV2.*
  import it.unibo.pslab.multiparty.MultiPartyV2
  // import it.unibo.pslab.multiparty.MultiPartyV2.*
  import it.unibo.pslab.network.mqtt.MqttNetwork
  // import it.unibo.pslab.network.memory.InMemoryNetwork

  type Client <: { type Tie <: through[MqttNetwork] toSingle Server }
  type Server <: { type Tie <: through[MqttNetwork] toSingle Client }

  def example[F[_]](using l: MultiPartyV2[F]) =
    l.comm[Client, Server]("Hello, Server!")
