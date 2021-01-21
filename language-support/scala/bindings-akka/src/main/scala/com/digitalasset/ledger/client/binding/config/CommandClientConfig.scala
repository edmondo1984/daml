// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.ledger.client.binding.config

import java.time.Duration

import com.daml.ledger.client.configuration.CommandClientConfiguration

final case class CommandClientConfig(
    maxCommandsInFlight: Int,
    maxParallelSubmissions: Int,
    defaultDeduplicationTime: Duration,
) {
  def toBindingConfig: CommandClientConfiguration =
    CommandClientConfiguration(
      maxCommandsInFlight = maxCommandsInFlight,
      maxParallelSubmissions = maxParallelSubmissions,
      defaultDeduplicationTime = defaultDeduplicationTime,
      interceptors = Seq.empty,
    )
}
