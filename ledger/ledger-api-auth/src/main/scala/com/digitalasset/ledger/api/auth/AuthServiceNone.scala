// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.ledger.api.auth

import java.util.concurrent.{CompletableFuture, CompletionStage}

import io.grpc.Metadata

/** An AuthService that rejects all calls by always returning an empty set of [[Claims]] */
object AuthServiceNone extends AuthService {
  override def decodeMetadata(headers: Metadata): CompletionStage[Claims] = {
    CompletableFuture.completedFuture(Claims.empty)
  }
}
