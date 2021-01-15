// Copyright (c) 2021 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0

package com.daml.script.dump

import java.nio.file.Files
import java.util.UUID

import com.daml.bazeltools.BazelRunfiles
import com.daml.lf.data.Ref.Identifier
import com.daml.lf.engine.script.Runner
import com.daml.ledger.api.domain
import com.daml.ledger.api.testing.utils.{AkkaBeforeAndAfterAll, SuiteResourceManagementAroundEach}
import com.daml.ledger.api.v1.command_service.SubmitAndWaitRequest
import com.daml.ledger.api.v1.commands._
import com.daml.ledger.api.v1.value._
import com.daml.ledger.client.LedgerClient
import com.daml.ledger.client.configuration.{CommandClientConfiguration, LedgerClientConfiguration, LedgerIdRequirement}
import com.daml.lf.archive.{DarReader, Decode}
import com.daml.platform.sandbox.services.TestCommands
import com.daml.platform.sandboxnext.SandboxNextFixture
import scalaz.syntax.tag._

import scala.sys.process._
import org.scalatest._
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

final class IT
    extends AsyncFreeSpec
    with Matchers
    with AkkaBeforeAndAfterAll
    with SuiteResourceManagementAroundEach
    with SandboxNextFixture
    with TestCommands {
  private val appId = domain.ApplicationId("script-dump")
  private val clientConfiguration = LedgerClientConfiguration(
    applicationId = appId.unwrap,
    ledgerIdRequirement = LedgerIdRequirement.none,
    commandClient = CommandClientConfiguration.default,
    sslContext = None,
    token = None,
  )
  // TODO cleanup
  private val tmpDir = Files.createTempDirectory("script_dump")
  private val damlc = BazelRunfiles.requiredResource("compiler/damlc/damlc")
  private val damlScriptLib = BazelRunfiles.requiredResource("daml-script/daml/daml-script.dar")

  "Generated dump for IOU transfer compiles" in {
    for {
      client <- LedgerClient(channel, clientConfiguration)
      p1 <- client.partyManagementClient.allocateParty(None, None).map(_.party)
      p2 <- client.partyManagementClient.allocateParty(None, None).map(_.party)
      t0 <- client.commandServiceClient.submitAndWaitForTransaction(
        SubmitAndWaitRequest(
          Some(
            Commands(
              ledgerId = client.ledgerId.unwrap,
              applicationId = appId.unwrap,
              commandId = UUID.randomUUID().toString(),
              party = p1,
              commands = Seq(
                Command().withCreate(
                  CreateCommand(
                    templateId = Some(
                      Identifier(
                        packageId = packageId,
                        moduleName = "Iou",
                        entityName = "Iou",
                      )
                    ),
                    createArguments = Some(
                      Record(
                        fields = Seq(
                          RecordField("issuer", Some(Value().withParty(p1))),
                          RecordField("owner", Some(Value().withParty(p1))),
                          RecordField("currency", Some(Value().withText("USD"))),
                          RecordField("amount", Some(Value().withNumeric("100"))),
                          RecordField("observers", Some(Value().withList(List()))),
                        )
                      )
                    ),
                  )
                )
              ),
            )
          )
        )
      )
      cid0 = t0.getTransaction.events(0).getCreated.contractId
      t1 <- client.commandServiceClient.submitAndWaitForTransaction(
        SubmitAndWaitRequest(
          Some(
            Commands(
              ledgerId = client.ledgerId.unwrap,
              applicationId = appId.unwrap,
              commandId = UUID.randomUUID().toString(),
              party = p1,
              commands = Seq(
                Command().withExercise(
                  ExerciseCommand(
                    templateId = Some(
                      Identifier(
                        packageId = packageId,
                        moduleName = "Iou",
                        entityName = "Iou",
                      )
                    ),
                    choice = "Iou_Split",
                    contractId = cid0,
                    choiceArgument = Some(
                      Value().withRecord(
                        Record(fields = Seq(RecordField(value = Some(Value().withNumeric("50")))))
                      )
                    ),
                  )
                )
              ),
            )
          )
        )
      )
      cid1 = t1.getTransaction.events(1).getCreated.contractId
      cid2 = t1.getTransaction.events(2).getCreated.contractId
      t2 <- client.commandServiceClient.submitAndWaitForTransaction(
        SubmitAndWaitRequest(
          Some(
            Commands(
              ledgerId = client.ledgerId.unwrap,
              applicationId = appId.unwrap,
              commandId = UUID.randomUUID().toString(),
              party = p1,
              commands = Seq(
                Command().withExercise(
                  ExerciseCommand(
                    templateId = Some(
                      Identifier(
                        packageId = packageId,
                        moduleName = "Iou",
                        entityName = "Iou",
                      )
                    ),
                    choice = "Iou_AddObserver",
                    contractId = cid2,
                    choiceArgument = Some(
                      Value().withRecord(
                        Record(fields = Seq(RecordField(value = Some(Value().withParty(p2)))))
                      )
                    ),
                  )
                )
              ),
            )
          )
        )
      )
      cid3 = t2.getTransaction.events(1).getCreated.contractId
      t3 <- client.commandServiceClient.submitAndWaitForTransaction(
        SubmitAndWaitRequest(
          Some(
            Commands(
              ledgerId = client.ledgerId.unwrap,
              applicationId = appId.unwrap,
              commandId = UUID.randomUUID().toString(),
              party = p1,
              commands = Seq(
                Command().withExercise(
                  ExerciseCommand(
                    templateId = Some(
                      Identifier(
                        packageId = packageId,
                        moduleName = "Iou",
                        entityName = "Iou",
                      )
                    ),
                    choice = "Iou_Transfer",
                    contractId = cid3,
                    choiceArgument = Some(
                      Value().withRecord(
                        Record(fields = Seq(RecordField(value = Some(Value().withParty(p2)))))
                      )
                    ),
                  )
                )
              ),
            )
          )
        )
      )
      cid4 = t3.getTransaction.events(1).getCreated.contractId
      _ <- client.commandServiceClient.submitAndWaitForTransaction(
        SubmitAndWaitRequest(
          Some(
            Commands(
              ledgerId = client.ledgerId.unwrap,
              applicationId = appId.unwrap,
              commandId = UUID.randomUUID().toString(),
              party = p2,
              commands = Seq(
                Command().withExercise(
                  ExerciseCommand(
                    templateId = Some(
                      Identifier(
                        packageId = packageId,
                        moduleName = "Iou",
                        entityName = "IouTransfer",
                      )
                    ),
                    choice = "IouTransfer_Accept",
                    contractId = cid4,
                    choiceArgument = Some(Value().withRecord(Record())),
                  )
                )
              ),
            )
          )
        )
      )
      _ <- Main.run(
        Config(
          ledgerHost = "localhost",
          ledgerPort = serverPort.value,
          parties = scala.List(p1, p2),
          outputPath = tmpDir,
          damlScriptLib = damlScriptLib.toString,
          sdkVersion = "0.0.0",
        )
      )

      _ = Seq[String](damlc.toString, "build", "--project-root", tmpDir.toString, "-o", tmpDir.resolve("dump.dar").toString).! shouldBe 0
      // Now run the DAML Script again
      dar = DarReader().readArchiveFromFile(tmpDir.resolve("dump.dar").toFile)
        .map
        { case (pkgId, pkgArchive) => Decode.readArchivePayload(pkgId, pkgArchive)
        }
      _ <- Runner.run(dar, Identifier())
    } yield succeed
  }
}
