package it.unibo.pslab.libraries

import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.Codable
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, PeerTag, TiedWithMultiple, TiedWithSingle }

import cats.Monad
import cats.syntax.all.*
import cats.data.NonEmptyList

object SyncLibrary:

  def requestReply[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: Monad, Request: Codable[F], Response: Codable[F]](request: Request)(
      handle: Request => F[Response],
  )(using MultiParty[F]): F[Response on From] =
    for
      reqOnRequester <- on[From](request.pure[F])
      reqOnResponder <- comm[From, To](reqOnRequester)
      resOnResponder <- on[To]:
        take(reqOnResponder) >>= handle
      resOnRequester <- comm[To, From](resOnResponder)
    yield resOnRequester

  def requestReplies[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: Monad, Request: Codable[F], Response: Codable[F]](using
      party: MultiParty[F],
  )(request: Request)(handle: Request => F[Response]): F[party.Anisotropic[To, Response] on From] =
    for
      reqOnRequester <- on[From](request.pure[F])
      reqOnResponder <- isotropicComm[From, To](reqOnRequester)
      resOnResponder <- on[To]:
        take(reqOnResponder) >>= handle
      resOnRequester <- coAnisotropicComm[To, From](resOnResponder)
    yield resOnRequester

  def scatterGather[From <: TiedWithMultiple[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: Monad, Task: Codable[F], PartialResult: Codable[F]](using
      party: MultiParty[F],
  )(
      allocator: NonEmptyList[party.Remote[To]] => Map[party.Remote[To], Task],
      default: Task,
  )(compute: Task => F[PartialResult]): F[party.Anisotropic[To, PartialResult] on From] =
    for
      messages <- on[From]:
        for
          peers <- reachablePeers[To]
          allocation = allocator(peers)
          message <- anisotropicMessage[From, To](allocation, default)
        yield message
      task <- anisotropicComm[From, To](messages)
      partialResult <- on[To]:
        take(task) >>= compute
      collectedResults <- coAnisotropicComm[To, From](partialResult)
    yield collectedResults
