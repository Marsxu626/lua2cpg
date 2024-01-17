package io.joern.lua2cpg

import io.joern.x2cpg.{X2CpgConfig, X2CpgMain}
import io.joern.x2cpg.passes.frontend.{TypeRecoveryParserConfig, XTypeRecovery}
import io.joern.lua2cpg.Frontend._
import scopt.OParser

final case  class Config(includePaths: Set[String] = Set.empty)
  extends X2CpgConfig[Config]
  with TypeRecoveryParserConfig[Config] {
  def withIncludePaths(includePaths: Set[String]): Config = {
    this.copy(includePaths = includePaths).withInheritedFields(this)
  }
}

private object Frontend {

  implicit val defaultConfig: Config = Config()

  val cmdLineParser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]
    import builder._
    OParser.sequence(
      programName("lua2cpg"),
      opt[String]("include")
        .unbounded()
        .text("header include paths")
        .action((incl, c) => c.withIncludePaths(c.includePaths + incl))
    )
  }
}

object Main extends X2CpgMain(cmdLineParser, new Lua2Cpg()) {
  def run(config: Config, lua2Cpg: Lua2Cpg): Unit = {
    lua2Cpg.run(config)
  }
}

