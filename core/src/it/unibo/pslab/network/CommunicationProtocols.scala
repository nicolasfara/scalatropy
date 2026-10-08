package it.unibo.pslab.network

type AnyProtocol = CommunicationProtocol
type * = CommunicationProtocol

trait MQTT extends CommunicationProtocol

trait WebSocket extends CommunicationProtocol

trait Memory extends CommunicationProtocol
