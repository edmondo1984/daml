// Copyright (c) 2020 The DAML Authors. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.digitalasset.platform.apiserver

import com.codahale.metrics.MetricRegistry

object MetricsNaming {

  private[this] val capitalization = "[A-Z]+".r
  private[this] val startWordCapitalization = "^[A-Z]+".r
  private[this] val endWordAcronym = "[A-Z]{2,}$".r

  private[this] val snakifyWholeWord = (s: String) => if (s.forall(_.isUpper)) s.toLowerCase else s

  private[this] val snakify = (s: String) =>
    capitalization.findAllMatchIn(s).foldRight(s) { (m, r) =>
      val s = m.toString
      if (s.length == 1) r.patch(m.start, s"_${s.toLowerCase}", 1)
      else r.patch(m.start, s"_${s.init.toLowerCase}_${s.last.toLower}", s.length)
  }

  private[this] val snakifyStart = (s: String) =>
    startWordCapitalization.findFirstIn(s).fold(s) { m =>
      s.patch(
        0,
        if (m.length == 1) m.toLowerCase else m.init.toLowerCase,
        math.max(m.length - 1, 1))
  }

  private[this] val snakifyEnd = (s: String) =>
    endWordAcronym.findFirstIn(s).fold(s) { m =>
      s.patch(s.length - m.length, s"_${m.toLowerCase}", m.length)
  }

  // Turns a camelCased string into a snake_cased one
  private[apiserver] val camelCaseToSnakeCase: String => String =
    snakifyWholeWord andThen snakifyStart andThen snakifyEnd andThen snakify

  // assert(fullServiceName("org.example.SomeService/someMethod") == "daml.lapi.some_service.some_method")
  private[apiserver] def nameFor(fullMethodName: String): String = {
    val serviceAndMethodName = fullMethodName.split('/')
    assert(
      serviceAndMethodName.length == 2,
      s"Expected service and method names separated by '/', got '$fullMethodName'")
    val serviceName = camelCaseToSnakeCase(serviceAndMethodName(0).split('.').last)
    val methodName = camelCaseToSnakeCase(serviceAndMethodName(1))
    MetricRegistry.name("daml", "lapi", serviceName, methodName)
  }

}
