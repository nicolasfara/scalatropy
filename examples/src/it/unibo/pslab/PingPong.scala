package it.unibo.pslab

import cats.syntax.all.*
import cats.effect.std.Console
import it.unibo.pslab.peers.Peers.*
import it.unibo.pslab.peers.Peers.Quantifier.*
import cats.Monad
// import it.unibo.pslab.multiparty.MultiPartyLanguage.*
import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.multiparty.Environment
// import cats.Id
import it.unibo.pslab.network.Network
import it.unibo.pslab.network.InMemoryNetwork
import cats.effect.IOApp
import cats.effect.IO
// import cats.Defer

object Multiparty:
  type Pinger <: { type Tie <: Single[Ponger] }
  type Ponger <: { type Tie <: Single[Pinger] }

  def pingPongProgram[F[_]: Monad: Console](using lang: MultiParty[F]): F[Unit] = for
    initial <- on[Pinger](0.pure)
    _ <- pingPong(initial)
  yield ()

  def pingPong[F[_]: Monad: Console](using lang: MultiParty[F])(initial: Int on Pinger): F[Unit] = for
    onPonger <- comm[Pinger, Ponger](initial)
    _ <- Console[F].println(s"Pinger sent value to Ponger")
    newCounter <- on[Ponger]:
      take(onPonger).map(_ + 1)
    newCounterOnPinger <- comm[Ponger, Pinger](newCounter)
    result <- on[Pinger]:
      take(newCounterOnPinger).map(_ + 1)
    _ <- Console[F].println(s"Ponger sent value to Pinger")
    _ <- pingPong(result)
  yield ()

// @main
// def main(): Unit =
//   val env = Environment.make[Id]
//   val network: Network[Id] = InMemoryNetwork.make[Id]
//   val lang = MultiParty.project[Id, Multiparty.Pinger](env, network)
//   val program = Multiparty.pingPongProgram[Id](using summon[Monad[Id]], summon[Console[Id]], lang)
//   program

object PingPongIOApp extends IOApp.Simple:
  override def run: IO[Unit] =
    val env = Environment.make[IO]
    val network: Network[IO] = InMemoryNetwork.make[IO]
    val lang = MultiParty.project[IO, Multiparty.Pinger](env, network)
    val program = Multiparty.pingPongProgram[IO](using summon[Monad[IO]], summon[Console[IO]], lang)
    program
