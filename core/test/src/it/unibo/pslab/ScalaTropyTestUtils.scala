package it.unibo.pslab

import scala.compiletime.testing.{ Error, typeCheckErrors }

import org.scalatest.matchers.should
import org.scalatest.flatspec.AnyFlatSpec

trait ScalaTropyTestUtils:
  self: should.Matchers =>

  inline def ScalaTropyProgram(inline code: String): TypeChecking =
    val full = (ScalaTropyTestUtils.commonImports + code).stripMargin
    println("===================")
    println(full)
    println("===================")
    TypeChecking(typeCheckErrors(full))

  class TypeChecking(errors: List[Error]):
    def shouldTypeCheck =
      errors shouldBe empty

    infix def shouldRaise(expectedErrorMessages: List[String]) =
      val errorMsg = errors.map(_.message).mkString
      expectedErrorMessages.foreach(em => errorMsg should include(em))

object ScalaTropyTestUtils:
  private inline val commonImports =
    """import _root_.it.unibo.pslab.peers.PeersV2.*
    |import _root_.it.unibo.pslab.multiparty.MultiPartyV2
    |import _root_.it.unibo.pslab.network.mqtt.MQTT
    |"""
