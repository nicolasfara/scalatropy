package it.unibo.pslab.libraries

import it.unibo.pslab.multiparty.MultiParty
import it.unibo.pslab.multiparty.MultiParty.*
import it.unibo.pslab.network.Codable
import it.unibo.pslab.peers.Peers.{ CommunicationProtocolCompliance, PeerTag, TiedWithMultiple, TiedWithSingle }

import cats.Monad
import cats.syntax.all.*
import cats.data.NonEmptyList

object SyncLibrary:

  /**
   * Implements a synchronous request-reply communication pattern between two peers, tied in a one-to-one relationship.
   *
   * @param request
   *   The request message to be sent from the requester to the responder. This usually conflates the request type and
   *   data needed for the request.
   * @param handle
   *   the effectful function that processes the request on the responder side and produces a response.
   * @return
   *   the response produced by the responder after processing the request placed on the requester.
   */
  def requestReply[From <: TiedWithSingle[To], To <: TiedWithSingle[From]](using
      PeerTag[From],
      PeerTag[To],
      CommunicationProtocolCompliance[From, To],
      CommunicationProtocolCompliance[To, From],
  )[F[_]: Monad, Request: Codable[F], Response: Codable[F]](using
      MultiParty[F],
  )(request: Request)(handle: Request => F[Response]): F[Response on From] =
    for
      reqOnRequester <- on[From](request.pure[F])
      reqOnResponder <- comm[From, To](reqOnRequester)
      resOnResponder <- on[To]:
        take(reqOnResponder) >>= handle
      resOnRequester <- comm[To, From](resOnResponder)
    yield resOnRequester

  /**
   * Implements a synchronous request-reply communication pattern between a peer and multiple peers, tied in a
   * one-to-many (1-n) relationship. The same request is broadcast to all responders, which individually process it and
   * send back their responses.
   *
   * @param request
   *   The request message to be broadcasted from the requester to the responders. This usually conflates the request
   *   type and data needed for the request.
   * @param handle
   *   the effectful function that processes the request on the responder side and produces a response.
   * @return
   *   the responses produced by the responders after processing the request placed on the requester.
   */
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

  /**
   * Implements a scatter-gather communication pattern, usually employed for realizing embarrassingly parallel
   * computations, where a task is distributed among multiple peers, each performing a part of the computation and
   * returning partial results that are then collected and aggregated. Aggregation is left open for the client to
   * implement.
   *
   * @param allocator
   *   The function responsible for allocating tasks to the available peers.
   * @param default
   *   The default task to be used in case a peer is not allocated a specific task.
   * @param compute
   *   The effectful function that performs the computation on the allocated task and produces a partial result.
   */
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
