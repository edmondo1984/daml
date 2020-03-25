// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.grpc.adapter.utils.implementations

import java.util.concurrent.atomic.AtomicInteger

import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Source}
import com.digitalasset.grpc.adapter.ExecutionSequencerFactory
import com.digitalasset.grpc.adapter.server.akka.ServerAdapter
import com.digitalasset.grpc.sampleservice.Responding
import com.digitalasset.platform.hello.HelloServiceGrpc.HelloService
import com.digitalasset.platform.hello.{HelloRequest, HelloResponse, HelloServiceGrpc}
import io.grpc.stub.StreamObserver
import io.grpc.{BindableService, ServerServiceDefinition}

import scala.concurrent.ExecutionContext.Implicits.global

class AkkaImplementation(
    implicit executionSequencerFactory: ExecutionSequencerFactory,
    materializer: Materializer,
) extends HelloService
    with Responding
    with BindableService {

  private val serverStreamingCalls = new AtomicInteger()

  def getServerStreamingCalls: Int = serverStreamingCalls.get()

  override def bindService(): ServerServiceDefinition =
    HelloServiceGrpc.bindService(this, global)

  override def serverStreaming(
      request: HelloRequest,
      responseObserver: StreamObserver[HelloResponse],
  ): Unit =
    Source
      .single(request)
      .via(Flow[HelloRequest].mapConcat(responses))
      .runWith(ServerAdapter.toSink(responseObserver))
      .onComplete(_ => serverStreamingCalls.incrementAndGet())

}
