// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

import com.daml.lf.PureCompiledPackages
import com.daml.lf.data
import com.daml.lf.language.Ast.{Expr, Package}
import com.daml.lf.speedy.Compiler.FullStackTrace
import com.daml.lf.speedy.SValue._
import com.daml.lf.speedy.SError._
import com.daml.lf.speedy.SResult.{SResultFinalValue, SResultError}
import com.daml.lf.testing.parser.Implicits._
import com.daml.lf.validation.Validation
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionTest extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  "basic throw/catch" should {

    val pkgs: PureCompiledPackages = noPackages

    val testCases = Table[String, Long](
      ("expression", "number"),
      ("42", 42),
      ("RUN_UPDATE (try @t (upure @t 42) catch e -> 99)", 42),
      ("RUN_UPDATE (try @t (upure @t (throw @t @t 11)) catch e -> Some @t (upure @t 99))", 99),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "unhandled throw" should {

    val pkgs: PureCompiledPackages = noPackages

    val testCases = Table[String, SResult](
      ("expression", "result"),
      ("42", SResultFinalValue(SInt64(42))),
      ("throw @t @t 111", SResultError(DamlEUserError("Unhandled-Throw"))),
    )

    forEvery(testCases) { (exp: String, expected: SResult) =>
      s"eval[$exp] --> $expected" in {
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  "throw-catch control flow" should {

    val pkgs: PureCompiledPackages = typeAndCompile(p"""
       module M {
         val myThrow : forall (a: *). (Text -> a) =
           /\ (a: *). \(mes : Text) ->
             throw @a @GeneralError (MAKE_GENERAL_ERROR mes);

         val isPayLoad : AnyException -> Text -> Bool = \(e: AnyException) (mes: Text) ->
           EQUAL @AnyException e (to_any_exception @GeneralError (MAKE_GENERAL_ERROR mes));

         val extractPayload : AnyException -> Int64 = \(e: AnyException) ->
           case (M:isPayLoad e "payload2") of True -> 2
| False -> case (M:isPayLoad e "payload3") of True -> 3
| False -> 1000000;

         val myCatch : forall (a: *). (Int64 -> a) -> (Text -> a) -> a =
           /\ (a: *). \ (handler: Int64 -> a) (body: Text -> a) ->
             RUN_UPDATE @a
              (try @a (upure @a (body "body-unitish"))
               catch e -> Some @(Update a) (upure @a (handler (M:extractPayload e))));

         val throwCatchTest : (Int64 -> Int64) = \ (x: Int64) ->
           (ADD_INT64 1000
            (M:myCatch @Int64 (\(pay : Int64) -> ADD_INT64 100 pay) (\(u : Text) ->
             ADD_INT64 2000
              (M:myCatch @Int64 (\(pay : Int64) -> ADD_INT64 200 pay) (\(u : Text) ->
               ADD_INT64 4000
                (case (EQUAL @Int64 x 1) of True -> x
      | False -> case (EQUAL @Int64 x 2) of True -> M:myThrow @Int64 "payload2"
      | False -> case (EQUAL @Int64 x 3) of True -> M:myThrow @Int64 "payload3"
      | False -> case (EQUAL @Int64 x 4) of True -> x
      | False -> 2000000))))));

       }
      """)

    val testCases = Table[String, Long](
      ("expression", "expected"),
      ("M:throwCatchTest 1", 7001),
      ("M:throwCatchTest 2", 3202),
      ("M:throwCatchTest 3", 3203),
      ("M:throwCatchTest 4", 7004),
    )

    forEvery(testCases) { (exp: String, num: Long) =>
      s"eval[$exp] --> $num" in {
        val expected: SResult = SResultFinalValue(SValue.SInt64(num))
        runExpr(pkgs)(e"$exp") shouldBe expected
      }
    }
  }

  private def noPackages: PureCompiledPackages = PureCompiledPackages(Map.empty).toOption.get

  private def typeAndCompile(pkg: Package): PureCompiledPackages = {
    val rawPkgs = Map(defaultParserParameters.defaultPackageId -> pkg)
    Validation.checkPackage(rawPkgs, defaultParserParameters.defaultPackageId, pkg)
    data.assertRight(
      PureCompiledPackages(rawPkgs, Compiler.Config.Default.copy(stacktracing = FullStackTrace))
    )
  }

  private def runExpr(pkgs1: PureCompiledPackages)(e: Expr): SResult = {
    Speedy.Machine.fromPureExpr(pkgs1, e).run()
  }

}
