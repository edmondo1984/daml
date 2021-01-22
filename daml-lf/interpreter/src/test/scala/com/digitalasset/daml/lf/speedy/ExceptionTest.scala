// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.lf.speedy

import com.daml.lf.PureCompiledPackages
import com.daml.lf.data
import com.daml.lf.language.Ast
import com.daml.lf.language.Ast.{Expr, Package}
import com.daml.lf.speedy.Compiler.FullStackTrace
import com.daml.lf.speedy.SError._
import com.daml.lf.speedy.SResult.{SResultFinalValue, SResultError}
import com.daml.lf.testing.parser.Implicits._
import com.daml.lf.validation.Validation
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionTest extends AnyWordSpec with Matchers with TableDrivenPropertyChecks {

  val pkg: Package =
    p"""
       module M {
         val myThrow : forall (a: *). (Unit -> a) =
           /\ (a: *). \(u : Unit) ->
             RAISE @a "myThrow";

         val myCatch : forall (a: *). (Unit -> a) -> a -> a =
           /\ (a: *). \ (handler: Unit -> a) (x: a) ->
             x; // No catching yet!
             //handler (); // always catching!

         val func : (Int64 -> Int64) = \ (x: Int64) ->
           (ADD_INT64 1000
            (M:myCatch @Int64 (\(u : Unit) -> 100)
             (ADD_INT64 10
              (case (EQUAL @Int64 (ADD_INT64 x 40) 42) of True -> M:myThrow @Int64 () | False -> x))));
       }
      """

  "func(1) -- no exception" in {
    val exp = e"M:func 1"
    val expected = Right(SValue.SInt64(1011))
    runExpr(exp) shouldBe expected
  }
  "func(2) -- exception thrown!!" in {
    val exp = e"M:func 2"
    //val expected = Right(SValue.SInt64(1100)) // TODO: when we implement exceptions
    val expected = Left(DamlEUserError("Raise:myThrow"))
    runExpr(exp) shouldBe expected
  }
  "func(3) -- no exception" in {
    val exp = e"M:func 3"
    val expected = Right(SValue.SInt64(1013))
    runExpr(exp) shouldBe expected
  }

  private def typeAndCompile(pkg: Ast.Package): PureCompiledPackages = {
    val rawPkgs = Map(defaultParserParameters.defaultPackageId -> pkg)
    Validation.checkPackage(rawPkgs, defaultParserParameters.defaultPackageId, pkg)
    data.assertRight(
      PureCompiledPackages(rawPkgs, Compiler.Config.Default.copy(stacktracing = FullStackTrace))
    )
  }

  val pkgs = typeAndCompile(pkg)

  private def runExpr(e: Expr): Either[SError, SValue] = {
    val machine = Speedy.Machine.fromPureExpr(pkgs, e)
    machine.run() match {
      case SResultFinalValue(v) => Right(v)
      case SResultError(e) => Left(e)
      case res => crash(s"runExpr, unexpected result $res")
    }
  }

  def crash[A](reason: String): A = throw new RuntimeException(reason)

}
