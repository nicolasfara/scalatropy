package it.unibo.pslab

// A playground where performing manual tests and having fun while developing :)

object ManualTest1:
  import it.unibo.pslab.peers.PeersV2.*
  import it.unibo.pslab.multiparty.MultiPartyV2
  import it.unibo.pslab.network.mqtt.MQTT
  import it.unibo.pslab.network.memory.Memory

  type Client <: { type Tie <: via[Memory toSingle Server] }
  type Server <: { type Tie <: via[Memory toSingle Client] & via[MQTT toMultiple Sensor] }
  type Sensor <: { type Tie <: via[MQTT toMultiple Server] }

  def example[F[_]](using l: MultiPartyV2[F]) =
    l.comm[Client, Server]("Hello, Server!")

/*

TypeRef(
  TermRef(
    TermRef(
      TermRef(
        TermRef(
          TermRef(
            ThisType(TypeRef(NoPrefix,module class <root>)),
            object it
          ),
          object unibo
        ),
        object pslab
      ),
      object network
    ),
    object memory
  ),
  trait Memory
),
TypeRef(
  ThisType(
    TypeRef(
      ThisType(
        TypeRef(NoPrefix,module class pslab)),module class ManualTest1$)),type Server)))
 */
