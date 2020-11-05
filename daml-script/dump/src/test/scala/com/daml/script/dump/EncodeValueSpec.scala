// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.script.dump

import com.daml.ledger.api.v1.value.{Identifier, Record, RecordField, Value}
import java.time.{Instant, LocalDate, OffsetDateTime, ZoneOffset}
import java.util.concurrent.TimeUnit

import org.scalatest._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class EncodeValueSpec extends AnyFreeSpec with Matchers {
  private def assertMicrosFromInstant(i: Instant): Long =
    TimeUnit.SECONDS.toMicros(i.getEpochSecond) + TimeUnit.NANOSECONDS.toMicros(i.getNano.toLong)

  import Encode._
  "encodeValue" - {
    "timestamp" in {
      val date = OffsetDateTime.of(1999, 11, 16, 13, 37, 42, 0, ZoneOffset.UTC)
      encodeValue(
        Map.empty,
        Map.empty,
        Value.Sum.Timestamp(assertMicrosFromInstant(date.toInstant())),
      ).render(80) shouldBe "(time (date 1999 Nov 16) 13 37 42)"
    }
    "date" in {
      val date = LocalDate.of(1999, 11, 16)
      encodeValue(Map.empty, Map.empty, Value.Sum.Date(date.toEpochDay().toInt))
        .render(80) shouldBe "(date 1999 Nov 16)"
    }
    "record" in {
      val id1 = Identifier("pkg-id", "M", "R1")
      val id2 = Identifier("pkg-id", "M", "R2")
      val r = Value.Sum.Record(
        Record(
          Some(id1),
          Seq(
            RecordField("a", Some(Value().withInt64(1))),
            RecordField(
              "b",
              Some(
                Value().withRecord(
                  Record(Some(id2), Seq(RecordField("c", Some(Value().withInt64(42)))))
                )
              ),
            ),
          ),
        )
      )
      encodeValue(Map.empty, Map.empty, r).render(80) shouldBe
        """M.R1 with
        |  a = 1
        |  b = M.R2 with
        |    c = 42""".stripMargin
    }
  }
}
