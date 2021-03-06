// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.ledger.client.services.acs

import akka.NotUsed
import akka.stream.scaladsl.{Keep, Source}
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.grpc.adapter.client.akka.ClientAdapter
import com.digitalasset.ledger.api.domain.LedgerId
import com.digitalasset.ledger.api.v1.active_contracts_service.ActiveContractsServiceGrpc.ActiveContractsServiceStub
import com.digitalasset.ledger.api.v1.active_contracts_service.{
  GetActiveContractsRequest,
  GetActiveContractsResponse
}
import com.digitalasset.ledger.api.v1.transaction_filter.TransactionFilter
import com.digitalasset.ledger.client.LedgerClient
import com.digitalasset.util.akkastreams.ExtractMaterializedValue
import scalaz.syntax.tag._

import scala.concurrent.Future

object ActiveContractSetClient {

  private val extractOffset =
    new ExtractMaterializedValue[GetActiveContractsResponse, String](r =>
      if (r.offset.nonEmpty) Some(r.offset) else None)

}

final class ActiveContractSetClient(ledgerId: LedgerId, service: ActiveContractsServiceStub)(
    implicit esf: ExecutionSequencerFactory) {

  import ActiveContractSetClient.extractOffset

  private def request(filter: TransactionFilter, verbose: Boolean) =
    GetActiveContractsRequest(ledgerId.unwrap, Some(filter), verbose)

  private def activeContractSource(
      request: GetActiveContractsRequest,
      token: Option[String]): Source[GetActiveContractsResponse, NotUsed] =
    ClientAdapter.serverStreaming(request, LedgerClient.stub(service, token).getActiveContracts)

  /**
    * Returns a stream of GetActiveContractsResponse messages. The materialized value will
    * be resolved to the offset that can be used as a starting offset for streaming transactions
    * via the transaction service.
    * If the stream completes before the offset can be set, the materialized future will
    * be failed with an exception.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  def getActiveContracts(
      filter: TransactionFilter,
      verbose: Boolean = false,
      token: Option[String] = None): Source[GetActiveContractsResponse, Future[String]] =
    activeContractSource(request(filter, verbose), token).viaMat(extractOffset)(Keep.right)

}
