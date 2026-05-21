package it.unibo.pslab.network

type AnyProtocol = CommunicationProtocol

trait MQTT extends CommunicationProtocol

trait WebSocket extends CommunicationProtocol

trait Memory extends CommunicationProtocol
