// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.daml.lf.engine.trigger

import java.nio.file.{Path, Paths}
import java.time.Duration

import com.digitalasset.platform.services.time.TimeProviderType

case class ServiceConfig(
    // For now, we only support one dar.
    darPath: Path,
    ledgerHost: String,
    ledgerPort: Int,
    timeProviderType: TimeProviderType,
    commandTtl: Duration,
)

object ServiceConfig {
  private val parser = new scopt.OptionParser[ServiceConfig]("trigger-service") {
    head("trigger-service")

    opt[String]("dar")
      .required()
      .action((f, c) => c.copy(darPath = Paths.get(f)))
      .text("Path to the dar file containing the trigger")

    opt[String]("ledger-host")
      .required()
      .action((t, c) => c.copy(ledgerHost = t))
      .text("Ledger hostname")

    opt[Int]("ledger-port")
      .required()
      .action((t, c) => c.copy(ledgerPort = t))
      .text("Ledger port")

    opt[Unit]('w', "wall-clock-time")
      .action { (t, c) =>
        c.copy(timeProviderType = TimeProviderType.WallClock)
      }
      .text("Use wall clock time (UTC). When not provided, static time is used.")

    opt[Long]("ttl")
      .action { (t, c) =>
        c.copy(commandTtl = Duration.ofSeconds(t))
      }
      .text("TTL in seconds used for commands emitted by the trigger. Defaults to 30s.")
  }
  def parse(args: Array[String]): Option[ServiceConfig] =
    parser.parse(
      args,
      ServiceConfig(
        darPath = null,
        ledgerHost = null,
        ledgerPort = 0,
        timeProviderType = TimeProviderType.Static,
        commandTtl = Duration.ofSeconds(30L),
      ),
    )
}
