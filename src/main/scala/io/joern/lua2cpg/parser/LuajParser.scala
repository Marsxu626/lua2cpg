package io.joern.lua2cpg.parser


import org.luaj.vm2.ast.*
import org.luaj.vm2.ast.Exp.BinopExp
import org.luaj.vm2.ast.Stat.{Assign, LocalAssign}
import org.luaj.vm2.parser.LuaParser
import org.luaj.vm2.ast.TableField
import io.joern.lua2cpg.parser.Domain.*
import scala.jdk.CollectionConverters.*
import java.io.{File, FileInputStream, InputStreamReader}
import play.api.libs.json.{JsArray, JsValue, Json}
import org.slf4j.LoggerFactory
import ujson.Value

import scala.io.Source
import scala.util.{Try, Using, Failure, Success}
class LuajParser(filePath: String) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  case class ParserResult(filename: String, fullPath: String, json: Value, fileContent: String)

  def parse(filePath: String): Option[LuaFile] = {
    val file = new File(filePath)
    val fileInputStream = new FileInputStream(file)
    val inputStreamReader = new InputStreamReader(fileInputStream, "UTF-8")
    val parser = new LuaParser(inputStreamReader)
    val ast = parser.Chunk
    val astJson: JsValue = chunkToJson(ast)
    val jsonString: String = Json.stringify(astJson("chunkbody"))
    val prettyJson: String = Json.prettyPrint(astJson)
    val json = ujson.read(jsonString)
    jsonValueToLuaFile(json, filePath)
  }
  private def jsonValueToLuaFile(json: ujson.Value, filename: String): Option[LuaFile] = {
    Try(Domain.fromJson(json)) match {
      case Success(luaFile) => Some(luaFile)
      case Failure(e) =>
        logger.error(s"Failed to generate intermediate AST %s",e )
        None
    }
  }
  private def chunkToJson(chunk: Chunk): JsValue = {
    val statist = chunk.block.stats.asScala.toList
    val statementsJson:List[JsValue] = statist.map{
      case stat: Stat =>
        statementToJson(stat)
      case _ => Json.obj("type" -> "Unknownstat")
    }
//    Json.obj("type" -> "Chunk", "body" -> statementsJson,"lineInfo"->lineInfo)
    Json.obj("chunkbody"->statementsJson)
    }
  // 处理 Stat列表
    private def statementToJson(statement: Stat): JsValue = statement match{
      //处理block
      case block: Block =>
        val bodyJson = block.stats.asScala.toList.map {
          case stat: Stat => statementToJson(stat)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toArray
        val lineInfo = Json.obj(
          "beginLine" -> block.beginLine,
          "beginColumn" -> block.beginColumn,
          "endLine" -> block.endLine,
          "endColumn" -> block.endColumn
        )
        Json.obj("type" -> "Block", "body" ->JsArray(bodyJson), "lineInfo"->lineInfo)
      //处理whileDo
      case whileDo: Stat.WhileDo =>
        val conditionJson = expressionToJson(whileDo.exp)
        val bodyJson = statementToJson(whileDo.block)
        val lineInfo = Json.obj(
          "beginLine" -> whileDo.beginLine,
          "beginColumn" -> whileDo.beginColumn,
          "endLine" -> whileDo.endLine,
          "endColumn" -> whileDo.endColumn
        )
        Json.obj("type" -> "WhileDo", "condition" -> conditionJson, "body" -> bodyJson,"lineInfo"->lineInfo)
      //处理RepeatUntil
      case until: Stat.RepeatUntil =>
        val bodyJson = statementToJson(until.block)
        val conditionJson = expressionToJson(until.exp)
        val lineInfo = Json.obj(
          "beginLine" -> until.beginLine,
          "beginColumn" -> until.beginColumn,
          "endLine" -> until.endLine,
          "endColumn" -> until.endColumn
        )
        Json.obj("type" -> "RepeatUntil", "body" -> bodyJson, "condition" -> conditionJson,"lineInfo"->lineInfo)
      //处理Return
      case value: Stat.Return =>
        val returnValuesJson = Option(value.values).map(_.asScala.toList).getOrElse(List.empty).map {
          case exp: Exp => expressionToJson(exp)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toArray
        val lineInfo = Json.obj(
          "beginLine" -> value.beginLine,
          "beginColumn" -> value.beginColumn,
          "endLine" -> value.endLine,
          "endColumn" -> value.endColumn
        )
        Json.obj("type" -> "Return", "values" -> returnValuesJson,"lineInfo"->lineInfo)
//      //处理FuncCallStat
      case stat: Stat.FuncCallStat =>
        val funcCallJson = expressionToJson(stat.funccall)
        val lineInfo = Json.obj(
          "beginLine" -> stat.beginLine,
          "beginColumn" -> stat.beginColumn,
          "endLine" -> stat.endLine,
          "endColumn" -> stat.endColumn
        )
        Json.obj("type" -> "FuncCallStat", "funccall" -> funcCallJson,"lineInfo"->lineInfo)
      //处理FuncDef
      case funcDef: Stat.FuncDef =>
        val nameJson = funcDef.name.name.name
        val bodyJson = funcBodyToJson(funcDef.body)
        val lineInfo = Json.obj(
          "beginLine" -> funcDef.beginLine,
          "beginColumn" -> funcDef.beginColumn,
          "endLine" -> funcDef.endLine,
          "endColumn" -> funcDef.endColumn
        )
        Json.obj("type" -> "FuncDef", "name" -> nameJson, "body" -> bodyJson,"lineInfo"->lineInfo)
      //处理LocalFuncDef
      case funcDef: Stat.LocalFuncDef =>
        val nameJson = funcDef.name.name
        val bodyJson = funcBodyToJson(funcDef.body)
        val lineInfo = Json.obj(
          "beginLine" -> funcDef.beginLine,
          "beginColumn" -> funcDef.beginColumn,
          "endLine" -> funcDef.endLine,
          "endColumn" -> funcDef.endColumn
        )
        Json.obj("type" -> "LocalFuncDef", "name" -> nameJson, "body" -> bodyJson,"lineInfo"->lineInfo)
      //处理GenericFor
      case genericFor: Stat.GenericFor =>
        val namesJson = genericFor.names.asScala.toList.map{
          case name: Name => variableToJson(name)
          case _ => Json.obj("type" -> "Unknownname")
        }.toArray
        val iteratorsJson = genericFor.exps.asScala.toList.map {
          case exp: Exp => expressionToJson(exp)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toArray
        val bodyJson = Json.arr(statementToJson(genericFor.block))
        val namespace = "genericFor"
        val lineInfo = Json.obj(
          "beginLine" -> genericFor.beginLine,
          "beginColumn" -> genericFor.beginColumn,
          "endLine" -> genericFor.endLine,
          "endColumn" -> genericFor.endColumn
        )
        Json.obj("type" -> "GenericFor", "names" -> namesJson, "iterators" -> iteratorsJson, "body" -> bodyJson,"scope"->namespace,"lineInfo"->lineInfo)
      //处理NumericFor
      case genericFor: Stat.NumericFor =>
        val nameJson = genericFor.name.name
        val initJson = Seq(expressionToJson(genericFor.initial))
        val limitJson = Seq(expressionToJson(genericFor.limit))
        val bodyJson = Seq(statementToJson(genericFor.block))
        val namespace = "numericFor"
        val lineInfo = Json.obj(
          "beginLine" -> genericFor.beginLine,
          "beginColumn" -> genericFor.beginColumn,
          "endLine" -> genericFor.endLine,
          "endColumn" -> genericFor.endColumn
        )
        val stepJson = genericFor.step match {
          case step: Exp => expressionToJson(step)
          case null => Json.obj("type" -> "Constant", "value" -> "1", "lineInfo" -> lineInfo)
        }
        Json.obj("type" -> "NumericFor", "name" -> nameJson, "init" -> initJson, "limit" -> limitJson, "step" -> Seq(stepJson), "body" -> bodyJson,"scope"->namespace,"lineInfo"->lineInfo)
      //处理IfThenElse
      case thenElse: Stat.IfThenElse =>
        val ifCondJson = expressionToJson(thenElse.ifexp)
        val ifBodyJson =  Option(thenElse.ifblock.asInstanceOf[Block].stats).map(_.asScala).getOrElse(List.empty).map{
          case stat: Stat => statementToJson(stat)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toList
//          statementToJson(thenElse.ifblock)
        val elseifCondJson =Option(thenElse.elseifexps).map(_.asScala).getOrElse(List.empty).map {
          case exp: Exp => expressionToJson(exp)
          case _ => Json.obj("type" -> "Unknown")
        }.toList
        val elseifBodyJson = Option(thenElse.elseifblocks).map(_.asScala).getOrElse(List.empty).map{
          case block: Block => statementToJson(block)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toList
        val elseBodyJson = Option(thenElse.elseblock).flatMap(elseBlock => Option(elseBlock.stats)).map(_.asScala).getOrElse(List.empty).map{
          case stat: Stat => statementToJson(stat)
          case _ => Json.obj("type" -> "Unknownstat")
        }.toList
        val lineInfo = Json.obj(
          "beginLine" -> thenElse.beginLine,
          "beginColumn" -> thenElse.beginColumn,
          "endLine" -> thenElse.endLine,
          "endColumn" -> thenElse.endColumn
        )
        Json.obj("type" -> "IfThenElse", "ifCond" -> ifCondJson, "ifBody" -> ifBodyJson,"elifCond"->elseifCondJson,"elifBody"->elseifBodyJson, "elseBody" -> elseBodyJson,"lineInfo"->lineInfo)
      //处理Goto
      case goto: Stat.Goto =>
        val labelJson = goto.name
        val lineInfo = Json.obj(
          "beginLine" -> goto.beginLine,
          "beginColumn" -> goto.beginColumn,
          "endLine" -> goto.endLine,
          "endColumn" -> goto.endColumn
        )
        Json.obj("type" -> "Goto", "label" -> labelJson,"lineInfo"->lineInfo)
      //处理Label
      case label: Stat.Label =>
        val labelJson = label.name
        val lineInfo = Json.obj(
          "beginLine" -> label.beginLine,
          "beginColumn" -> label.beginColumn,
          "endLine" -> label.endLine,
          "endColumn" -> label.endColumn
        )
        Json.obj("type" -> "Label", "label" -> labelJson,"lineInfo"->lineInfo)
      //处理Break
      case break: Stat.Break =>
        val lineInfo = Json.obj(
          "beginLine" -> break.beginLine,
          "beginColumn" -> break.beginColumn,
          "endLine" -> break.endLine,
          "endColumn" -> break.endColumn
        )
        Json.obj("type" -> "Break","lineInfo"->lineInfo)
      //处理LocalAssign
      case assign: LocalAssign =>
          val assignType = assign.names.asScala.toList.map{
            case name: Name => NameToJson(name)
            case _ => Json.obj("type" -> "Unknownname")
          }
          val assignInit = Option(assign.values).map(_.asScala.toList).getOrElse(List.empty).map{
            case exp: Exp => expressionToJson(exp)
            case _ => Json.obj("type" -> "Unknown")
          }
          val lineInfo = Json.obj(
          "beginLine" -> assign.beginLine,
          "beginColumn" -> assign.beginColumn,
          "endLine" -> assign.endLine,
          "endColumn" -> assign.endColumn
        )
          Json.obj("type" -> "LocalAssign", "variables" -> assignType, "init" -> assignInit,"lineInfo"->lineInfo)
      //处理Assign
      case assign: Assign =>
        val assignType = assign.vars.asScala.toList.map {
          case name: Name => NameToJson(name)
          case _ => Json.obj("type" -> "Unknownname")
        }
        val assignInit = assign.exps.asScala.toList.map {
          case exp: Exp => expressionToJson(exp)
          case _ => Json.obj("type" -> "Unknown")
        }
        val lineInfo = Json.obj(
          "beginLine" -> assign.beginLine,
          "beginColumn" -> assign.beginColumn,
          "endLine" -> assign.endLine,
          "endColumn" -> assign.endColumn
        )
        Json.obj("type" -> "Assign", "variables" -> assignType, "init" -> assignInit,"lineInfo"->lineInfo)
      //处理unknown
      case _ => Json.obj("type" -> "Unknownvar")
    }
  //处理Name列表
    private def variableToJson(variable: Name): JsValue ={
      val lineInfo = Json.obj(
        "beginLine" -> 0,
        "beginColumn" -> 0,
        "endLine" -> 0,
        "endColumn" -> 0
      )
      Json.obj("type" -> "VarExp", "name" -> variable.name,"lineInfo"->lineInfo)
    }
//处理函数参数
  private def paramToJson(param: Name, isvararg:Boolean, lineInfo:JsValue): JsValue = {
    Json.obj("type" -> "Param", "name" -> param.name,"isvararg"->isvararg, "lineInfo" -> lineInfo)
  }
  private def NameToJson(name: Name): JsValue = {
    val lineInfo = Json.obj(
      "beginLine" -> 0,
      "beginColumn" -> 0,
      "endLine" -> 0,
      "endColumn" -> 0
    )
    Json.obj("type" -> "VarExp", "name" -> name.name, "lineInfo" -> lineInfo)
  }
  //处理funcBody
  private def funcBodyToJson(funcBody: FuncBody): JsValue = {
    val lineInfo = Json.obj(
      "beginLine" -> funcBody.parlist.beginLine,
      "beginColumn" -> funcBody.parlist.beginColumn,
      "endLine" -> funcBody.parlist.endLine,
      "endColumn" -> funcBody.parlist.endColumn
    )
    val names = funcBody.parlist.names
    val parameters = Option(names).map(_.asScala.toList).getOrElse(List.empty).map{
//    val parameters = funcBody.parlist.names.asScala.toList.map{
      case name: Name => paramToJson(name,funcBody.parlist.isvararg,lineInfo)
      case _ => Json.obj("type" -> "Unknownname")
    }
    val statements = funcBody.block.asInstanceOf[Block].stats.asScala.toList.map{
      case stat: Stat => statementToJson(stat)
      case _ => Json.obj("type" -> "Unknownstat")
    }
    val lineInfo2 = Json.obj(
      "beginLine" -> funcBody.beginLine,
      "beginColumn" -> funcBody.beginColumn,
      "endLine" -> funcBody.endLine,
      "endColumn" -> funcBody.endColumn
    )
    Json.obj("params" -> parameters, "stmts" -> statements,"lineInfo"->lineInfo2)
  }

  //处理tablefiled
  private def tableFieldToJson(tableField: TableField): JsValue = {
    val rhsJson = expressionToJson(tableField.rhs)
    val lineInfo = Json.obj(
      "beginLine" -> tableField.beginLine,
      "beginColumn" -> tableField.beginColumn,
      "endLine" -> tableField.endLine,
      "endColumn" -> tableField.endColumn
    )
    if (tableField.index != null && tableField.name == null && tableField.rhs != null) {
      // KeyedField 类型
      val indexJson = expressionToJson(tableField.index)
      Json.obj("type" -> "KeyedField", "index" -> indexJson, "rhs" -> rhsJson,"lineInfo"->lineInfo)
    } else if (tableField.index == null && tableField.name != null && tableField.rhs != null) {
      // NamedField 类型
      val nameJson = Json.toJson(tableField.name)
      Json.obj("type" -> "NamedField", "name" -> nameJson, "rhs" -> rhsJson,"lineInfo"->lineInfo)
    } else if (tableField.index == null && tableField.name == null && tableField.rhs != null) {
      // ListField 类型
      Json.obj("type" -> "ListField", "rhs" -> rhsJson,"lineInfo"->lineInfo)
    } else {
      // 无法确定类型
      Json.obj("type" -> "Unknowntable")
    }
  }
  //处理Exp列表
  private def expressionToJson(expression: Exp): JsValue = expression match {
    case exp:Exp.NameExp =>
      val expName = exp.name.name
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
//      val scopeName = scope.outerScope
      Json.obj("type" -> "VarExp", "name" -> expName,"lineInfo"->lineInfo)
    case exp:Exp.FieldExp =>
      val table = exp.lhs
      val fieldName = exp.name.name
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      Json.obj("type" -> "FieldExp", "table" -> expressionToJson(table), "field" -> fieldName,"lineInfo"->lineInfo)
    case exp:Exp.IndexExp =>
      val table = exp.lhs
      val index = exp.exp
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      Json.obj("type" -> "IndexExp", "table" -> expressionToJson(table), "index" -> expressionToJson(index),"lineInfo"->lineInfo)
    case exp:Exp.ParensExp =>
      val innerExp = exp.exp
      val innerJson = expressionToJson(innerExp)
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      Json.obj("type" -> "ParensExp", "innerExp" -> innerJson,"lineInfo"->lineInfo)
    case exp: Exp.UnopExp =>
      val operator = exp.op
      val rhsJson = expressionToJson(exp.rhs)
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      Json.obj("type" -> "UnOpExp", "operator" -> operator, "rhs" -> rhsJson,"lineInfo"->lineInfo)
    case const: Exp.Constant =>
      val lineInfo = Json.obj(
        "beginLine" -> const.beginLine,
        "beginColumn" -> const.beginColumn,
        "endLine" -> const.endLine,
        "endColumn" -> const.endColumn
      )
      val   value:String =  const.value.toString
      Json.obj("type" -> "Constant", "value" -> value, "lineInfo"->lineInfo)
    case binaryOp: Exp.BinopExp =>
      val lineInfo = Json.obj(
        "beginLine" -> binaryOp.beginLine,
        "beginColumn" -> binaryOp.beginColumn,
        "endLine" -> binaryOp.endLine,
        "endColumn" -> binaryOp.endColumn
      )
      Json.obj("type" -> "BinaryOpExp", "operator" -> binaryOp.op, "left" -> expressionToJson(binaryOp.lhs), "right" -> expressionToJson(binaryOp.rhs),"lineInfo"->lineInfo)
    case funcCall: Exp.MethodCall =>
      val lhsJson = expressionToJson(funcCall.lhs)
      val funcName= funcCall.name
      val exps = funcCall.args.exps
      val argsJson = Option(exps).map(_.asScala.toList).getOrElse(List.empty).map {
        case exp: Exp => expressionToJson(exp)
        case _ => Json.obj("type" -> "Unknown")
      }
      val lineInfo = Json.obj(
        "beginLine" -> funcCall.beginLine,
        "beginColumn" -> funcCall.beginColumn,
        "endLine" -> funcCall.endLine,
        "endColumn" -> funcCall.endColumn
      )
      Json.obj("type" -> "MethodCall","lhs"->lhsJson, "name" -> funcName, "args" -> argsJson, "lineInfo" -> lineInfo)
    case funcCall: Exp.FuncCall =>
      val lhsJson = expressionToJson(funcCall.lhs)
      val exps = funcCall.args.exps
      val argsJson = Option(exps).map(_.asScala.toList).getOrElse(List.empty).map {
        case exp: Exp => expressionToJson(exp)
        case _ => Json.obj("type" -> "Unknown")
      }
      val lineInfo = Json.obj(
        "beginLine" -> funcCall.beginLine,
        "beginColumn" -> funcCall.beginColumn,
        "endLine" -> funcCall.endLine,
        "endColumn" -> funcCall.endColumn
      )
      Json.obj("type" -> "FuncCall", "lhs" -> lhsJson, "args" -> argsJson, "lineInfo" -> lineInfo)
    case exp: Exp.PrimaryExp =>
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      val expressionType = if (exp.isvarexp()) {
        "VarExp"
      } else if (exp.isfunccall()) {
        "FuncCall"
      } else {
        "Unknown"
      }
      Json.obj("type" -> "PrimaryExp", "expressionType" -> expressionType, "lineInfo" -> lineInfo)
    //处理函数调用
    case exp: Exp.VarargsExp =>
      val lineInfo = Json.obj(
        "beginLine" -> exp.beginLine,
        "beginColumn" -> exp.beginColumn,
        "endLine" -> exp.endLine,
        "endColumn" -> exp.endColumn
      )
      Json.obj("type" -> "VarargsExp","lineInfo"->lineInfo)
    case funcDef: Exp.AnonFuncDef =>
      val lineInfo = Json.obj(
        "beginLine" -> funcDef.beginLine,
        "beginColumn" -> funcDef.beginColumn,
        "endLine" -> funcDef.endLine,
        "endColumn" -> funcDef.endColumn
      )
      val bodyJson = funcBodyToJson(funcDef.body)
      Json.obj("type" -> "AnonFuncDef", "body" -> bodyJson,"lineInfo"->lineInfo)
    case constructor: TableConstructor =>
      val fieldsJson = Option(constructor.fields).map(_.asScala.toList).getOrElse(List.empty).map {
        case tableField: TableField => tableFieldToJson(tableField)
        case _ => Json.obj("type" -> "Unknowntablefield")
      }.toArray
      val lineInfo = Json.obj(
        "beginLine" -> constructor.beginLine,
        "beginColumn" -> constructor.beginColumn,
        "endLine" -> constructor.endLine,
        "endColumn" -> constructor.endColumn
      )
      Json.obj("type" -> "TableConstructor", "fields" -> fieldsJson, "lineInfo" -> lineInfo)
    case _ => Json.obj("type" -> "Unknownexp") // 未知表达式类型
  }
}