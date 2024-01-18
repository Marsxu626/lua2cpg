package io.joern.lua2cpg

import io.joern.lua2cpg.passes.{AstCreationPass,LocalCreationPass,AnyTypePass,LuaTypeRecoveryPass,LuaSetKnownTypesPass}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.codepropertygraph.generated.Languages
import org.slf4j.LoggerFactory

import scala.util.Try

class Lua2Cpg extends X2CpgFrontend[Config] {

//  private val report: Report = new Report()

  def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      new MetaDataPass(cpg, "Lua", config.inputPath).createAndApply()
      new AstCreationPass(cpg, config).createAndApply()
      new AnyTypePass(cpg).createAndApply()
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
      LocalCreationPass.allLocalCreationPasses(cpg).foreach(_.createAndApply())
      println("Success!")
    }
  }
}

object Lua2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Option[Config] = None): List[CpgPassBase] = {
    val typeRecoveryConfig = config
      .map(c => XTypeRecoveryConfig(c.typePropagationIterations, !c.disableDummyTypes))
      .getOrElse(XTypeRecoveryConfig(iterations = 3))
    List(new LuaSetKnownTypesPass(cpg), new LuaTypeRecoveryPass(cpg, typeRecoveryConfig))
  }
}