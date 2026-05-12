package it.unibo.pslab

import scala.util.Random

import it.unibo.pslab.ScalaTropy.*
import it.unibo.pslab.UpickleCodable.given
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.AnyProtocol
import it.unibo.pslab.network.mqtt.MqttNetwork
import it.unibo.pslab.network.mqtt.MqttNetwork.Configuration
import it.unibo.pslab.peers.Peers.*

import cats.MonadThrow
import cats.effect.{ IO, IOApp }
import cats.effect.std.Console
import cats.syntax.all.*
import upickle.default.ReadWriter

import CardGame.*

object CardGame:
  type Dealer <: { type Tie <: via[AnyProtocol toMultiple Player] }
  type Player <: { type Tie <: via[AnyProtocol toSingle Dealer] }

  final case class Card(value: Int) derives ReadWriter:
    def +(other: Card): Card = Card.of(value + other.value)
    def >(other: Card): Boolean = value > other.value
    override def toString: String = s"Card($value)"

  object Card:
    def of(value: Int): Card = Card(((value % 21) + 21) % 21)

  final case class HandDeal(initial: Card, second: Option[Card]) derives ReadWriter
  final case class PlayerResult(total: Card, won: Boolean) derives ReadWriter

  def cardGameEntrypoint[F[_]: {MonadThrow, Console}](using MultiParty[F]): F[Unit] =
    for
      deck <- on[Dealer](Seq.fill(10)(Random.nextInt).toList.pure)
      choice <- on[Player] {
        val secondCard = Random.nextBoolean()
        F.println(s"[Player] Do you want a second card? $secondCard")
          .as(secondCard) // For simplicity, we hardcode the choice here
      }
      _ <- cardGameProgram(deck, choice)
    yield ()

  def cardGameProgram[F[_]: {MonadThrow, Console}](
      deck: List[Int] on Dealer,
      wantsSecondCard: Boolean on Player,
  )(using MultiParty[F]): F[Unit] =
    for
      _ <- on[Player]:
        take(wantsSecondCard) >>= { choice =>
          F.println(s"[Player] Chose to ${if choice then "take" else "not take"} a second card")
        }
      choicesOnDealer <- coAnisotropicComm[Player, Dealer](wantsSecondCard)
      dealsOnDealer <- on[Dealer]:
        for
          choices <- takeAll(choicesOnDealer)
          deck <- take(deck)
          _ <- F.println(s"[Dealer] Hidden deck: ${deck.map(Card.of).mkString(", ")}")
          allocation = dealHands(deck, choices)
          _ <- F.println(s"[Dealer] Player choices: ${choices.view.mapValues(_.toString).toMap}")
          message <- anisotropicMessage[Dealer, Player](allocation.deals, HandDeal(Card.of(0), None))
        yield message
      handOnPlayer <- anisotropicComm[Dealer, Player](dealsOnDealer)
      tableCardOnDealer <- on[Dealer]:
        for
          choices <- takeAll(choicesOnDealer)
          deck <- take(deck)
          allocation = dealHands(deck, choices)
          _ <- F.println(s"[Dealer] Table card: ${allocation.tableCard}")
        yield allocation.tableCard
      tableCardOnPlayer <- isotropicComm[Dealer, Player](tableCardOnDealer)
      resultOnPlayer <- on[Player]:
        for
          hand <- take(handOnPlayer)
          tableCard <- take(tableCardOnPlayer)
          total = (tableCard :: (hand.second.toList :+ hand.initial)).reduce(_ + _)
          result = PlayerResult(total, total > Card.of(19))
          _ <- F.println(s"[Player] Hand: $hand, table: $tableCard, total: $total, win: ${result.won}")
        yield result
      resultsOnDealer <- coAnisotropicComm[Player, Dealer](resultOnPlayer)
      _ <- on[Dealer]:
        for
          results <- takeAll(resultsOnDealer)
          _ <- F.println(s"[Dealer] Final results: ${results.toList.sortBy(_._1.toString).mkString(", ")}")
        yield ()
    yield ()

  final private case class Allocation[RemotePlayer](
      deals: Map[RemotePlayer, HandDeal],
      tableCard: Card,
  )

  private def dealHands[F[_]](using
      lang: MultiParty[F],
  )(
      deck: List[Int],
      choices: Map[lang.Remote[Player], Boolean],
  ): Allocation[lang.Remote[Player]] =
    val cards = LazyList
      .continually(deck match
        case Nil      => List(0)
        case nonEmpty => nonEmpty)
      .flatten
      .map(Card.of)
    var deckIndex = 0
    val orderedPlayers = choices.toList.sortBy(_._1.toString)
    val deals = orderedPlayers.map: (player, wantsSecond) =>
      val initial = cards(deckIndex)
      deckIndex += 1
      val second = Option.when(wantsSecond):
        val card = cards(deckIndex)
        deckIndex += 1
        card
      player -> HandDeal(initial, second)
    Allocation(deals.toMap, cards(deckIndex))

object LaunchAll extends IOApp.Simple:
  override def run: IO[Unit] =
    List(
      CardGameDealerV2.run,
      CardGamePlayer1V2.run,
      CardGamePlayer2V2.run,
      CardGamePlayer3V2.run,
    ).parSequence_

object CardGameDealerV2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Dealer](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(cardGameEntrypoint[IO]).projectedOn[Dealer]:
        tiedTo[Player] via mqtt

object CardGamePlayer1V2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Player](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(cardGameEntrypoint[IO]).projectedOn[Player]:
        tiedTo[Dealer] via mqtt

object CardGamePlayer2V2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Player](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(cardGameEntrypoint[IO]).projectedOn[Player]:
        tiedTo[Dealer] via mqtt

object CardGamePlayer3V2 extends IOApp.Simple:
  override def run: IO[Unit] =
    val mqttNetwork = MqttNetwork.localBroker[IO, Player](Configuration(appId = "cardgame-v2"))
    mqttNetwork.use: mqtt =>
      ScalaTropy(cardGameEntrypoint[IO]).projectedOn[Player]:
        tiedTo[Dealer] via mqtt
