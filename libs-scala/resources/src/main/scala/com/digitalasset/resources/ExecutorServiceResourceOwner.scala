// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.resources

import java.util.concurrent.{ExecutorService, TimeUnit}

import com.digitalasset.resources.ExecutorServiceResourceOwner._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

class ExecutorServiceResourceOwner[T <: ExecutorService](acquireExecutorService: () => T)
    extends ResourceOwner[T] {
  override def acquire()(implicit executionContext: ExecutionContext): Resource[T] =
    Resource(Future {
      val executorService = acquireExecutorService()
      // If we try and release an executor service which is itself being used to power the
      // releasing, we end up in a deadlock—the executor can't shut down, and therefore
      // `awaitTermination `never completes. We mitigate this by attempting to catch the problem
      // early and fail with a meaningful exception.
      executionContext match {
        case context: ExecutionContextExecutorService =>
          if (executorService == context) {
            throw new CannotAcquireExecutionContext()
          }
          // Ugly, but important so that we make sure we're not going to end up in deadlock.
          // This calls a method on the private `ExecutionContextExecutorServiceImpl` class to get
          // the underlying executor, then compares it. If you're using a custom
          // `ExecutionContextExecutorService`, it probably won't follow the same API, so this
          // will silently ignore it. This means it'll acquire happily but deadlock on release.
          // We can't cover everything in these checks, unfortunately.
          try {
            val contextExecutor =
              context.getClass.getMethod("executor").invoke(context).asInstanceOf[ExecutorService]
            if (executorService == contextExecutor) {
              throw new CannotAcquireExecutionContext()
            }
          } catch {
            case _: NoSuchMethodException =>
          }
        case context: ExecutorService =>
          if (executorService == context) {
            throw new CannotAcquireExecutionContext()
          }
        case _ =>
      }
      executorService
    })(
      executorService =>
        Future {
          executorService.shutdown()
          val _ = executorService.awaitTermination(Long.MaxValue, TimeUnit.SECONDS)
      }
    )
}

object ExecutorServiceResourceOwner {

  class CannotAcquireExecutionContext
      extends RuntimeException(
        "The execution context used by resource acquisition cannot itself be acquired. This is to prevent deadlock upon release.")

}
