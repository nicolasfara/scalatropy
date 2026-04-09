package it.unibo.pslab

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import scala.compiletime.testing.{ Error, typeCheckErrors }

class MultiPartyV2ArchitecturalChecks extends AnyFlatSpec with should.Matchers:

  "comm" should "guarantee to compile when architecture and communication protocol are coherent" in:
    """
    | import _root_.it.unibo.pslab.peers.PeersV2.*
    | import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    | import _root_.it.unibo.pslab.network.mqtt.MQTT
    | 
    | type Client <: { type Tie <: via[MQTT] toSingle Server }
    | type Server <: { type Tie <: via[MQTT] toSingle Client }
    |
    | def example[F[_]](using l: MultiPartyV2[F]) =
    |   l.comm[Client, Server]("Hello, Server!")
    |   ???
    """.stripMargin should compile

  "comm" should "guarantee to **NOT** compile when architecture and communication protocol are incoherent" in:
    val compileErrors: List[Error] = typeCheckErrors("""
    | import _root_.it.unibo.pslab.peers.PeersV2.*
    | import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    | import _root_.it.unibo.pslab.network.mqtt.MQTT
    | import _root_.it.unibo.pslab.network.memory.Memory
    | 
    | type Client <: { type Tie <: via[MQTT] toSingle Server }
    | type Server <: { type Tie <: via[Memory] toSingle Client }
    |
    | def example[F[_]](using l: MultiPartyV2[F]) =
    |   l.comm[Client, Server]("Hello, Server!")
    |   ???
    """.stripMargin)
    compileErrors should not be empty
    errors.map(_.message).mkString should include("no common communication protocol found")
