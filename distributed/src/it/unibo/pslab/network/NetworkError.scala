package it.unibo.pslab.network

import scala.util.control.NoStackTrace

import it.unibo.pslab.peers.Peers.PeerTag

trait NetworkError(message: String) extends NoStackTrace:
  override def getMessage: String = message

case class NoSuchPeers(tag: PeerTag[?]) extends NetworkError(s"No alive peers of type $tag found")
