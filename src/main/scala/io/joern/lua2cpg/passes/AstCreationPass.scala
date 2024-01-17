package io.joern.lua2cpg.passes

import better.files.File
import io.joern.lua2cpg.parser.LuajParser
import io.joern.lua2cpg.astcreation.AstCreator
import io.joern.x2cpg.{SourceFiles, ValidationMode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.joern.lua2cpg.Config
import org.luaj.vm2.ast.Chunk
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters.*

class AstCreationPass(cpg: Cpg, config: Config) extends ConcurrentWriterCpgPass[String](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  private val LuaSourceFileExtensions: Set[String] = Set(".lua")

  override def generateParts(): Array[String] = SourceFiles.determine(config.inputPath, LuaSourceFileExtensions).toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, filename: String): Unit = {
    val relativeFilename = if (filename == config.inputPath) {
      File(filename).name
    } else {
      File(config.inputPath).relativize(File(filename)).toString
    }
    //解析文件，这里先用luaj-jse工具转成ast
    val parser = new LuajParser(filename)
    val result = parser.parse(filename)
    result match {
      case Some(result) =>
        val localDiff = new AstCreator(filename,result)(config.schemaValidation).createAst()
        diffGraph.absorb(localDiff)
      case None =>
        logger.warn(s"Could not parse file $filename. Results will be missing!")
    }
  }
}

