package it.unibo.pslab

// A playground where performing manual tests and having fun while developing!
object ManualTest1:
  import it.unibo.pslab.peers.PeersV2.*
  import it.unibo.pslab.multiparty.MultiPartyV2
  // import it.unibo.pslab.multiparty.MultiPartyV2.*
  import it.unibo.pslab.network.mqtt.MQTT
  import it.unibo.pslab.network.memory.Memory

  type Client <: { type Tie <: through[Memory toSingle Server] } // via[MQTT] toSingle
  type Server <: { type Tie <: through[MQTT toSingle Client] & through[Memory toMultiple Sensor] }
  type Sensor <: { type Tie <: via[Memory] toSingle Server }

  def example[F[_]](using l: MultiPartyV2[F]) =
    l.comm[Client, Server]("Hello, Server!")

object ManualTest2:
  import it.unibo.pslab.peers.PeersV2.*
  import it.unibo.pslab.multiparty.MultiPartyV2
  import it.unibo.pslab.network.mqtt.MQTT
  import it.unibo.pslab.network.memory.Memory

  type Client <: { type Tie <: via[MQTT] toSingle Server }
  type Server <: { type Tie <: via[Memory] toSingle Client }

  def example[F[_]](using l: MultiPartyV2[F]) =
    // l.comm[Client, Server]("Hello, Server!")
    ???
