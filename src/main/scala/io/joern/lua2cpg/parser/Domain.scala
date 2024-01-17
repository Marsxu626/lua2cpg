package io.joern.lua2cpg.parser

import ujson.{Arr, Obj, Str, Value}
import scala.util.{Success, Try}
import io.shiftleft.codepropertygraph.generated.*
import scala.collection.immutable.Map
import org.slf4j.LoggerFactory
object Domain {
  //节点的基本行列信息
  final case class LuaAttributes(beginLine: Option[Integer],beginColumn: Option[Integer],endLine: Option[Integer],endColumn: Option[Integer])
  object LuaAttributes {
    val Empty: LuaAttributes = LuaAttributes(None,None,None,None)
    def apply(json: Value): LuaAttributes = {
      Try(json("lineInfo")) match {
        case Success(Obj(lineInfo)) =>
          val beginLine = lineInfo.get("beginLine").map(num => Integer.valueOf(num.num.toInt))
          val beginColumn = lineInfo.get("beginColumn").map(num => Integer.valueOf(num.num.toInt))
          val endLine = lineInfo.get("endLine").map(num => Integer.valueOf(num.num.toInt))
          val endColumn = lineInfo.get("endColumn").map(num => Integer.valueOf(num.num.toInt))
          LuaAttributes(beginLine, beginColumn, endLine,endColumn)
        case Success(Arr(_)) =>
          logger.debug(s"Found array attributes in $json")
          LuaAttributes.Empty
        case unhandled =>
          logger.warn(s"Could not find attributes object in type $unhandled")
          LuaAttributes.Empty
      }
    }
  }
  private val logger          = LoggerFactory.getLogger(Domain.getClass)
  sealed trait LuaNode {
    def attributes: LuaAttributes
  }
  final case class LuaFile(children: List[LuaStmt]) extends LuaNode {
    override val attributes: LuaAttributes = LuaAttributes.Empty
  }
  sealed trait LuaStmt extends LuaNode
  sealed trait LuaStmtWithBody extends LuaStmt {
    def stmts: List[LuaStmt]
  }
//各个Stmt类型的声明
  final case class LuaBlockStmt(stmts: List[LuaStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaWhileDoStmt(cond: LuaExpr, stmts: List[LuaStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaRepeatUntilStmt(cond: LuaExpr, stmts: List[LuaStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaReturnStmt(expr: List[LuaExpr], attributes: LuaAttributes)    extends LuaStmt
  final case class LuaFuncCallStmt(funccall: LuaExpr, attributes: LuaAttributes)    extends LuaStmt
  //对函数定义节点处理
  final case class LuaFuncDefStmt(name: LuaExpr, params: List[LuaParamStmt], stmts: List[LuaStmt], returnByRef: Boolean, namespacedName: Option[LuaName], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LualocalFuncDefStmt(name: LuaExpr,
                   params: List[LuaParamStmt],
                   stmts: List[LuaStmt],
                   returnByRef: Boolean,
                   namespacedName: Option[LuaName],
                   attributes: LuaAttributes) extends LuaStmtWithBody
  final case class LuaGenericForStmt(names: List[LuaExpr], iterExpr: List[LuaExpr], stmts: List[LuaStmt], namespacedName: Option[LuaNamespaceStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaNumericForStmt(name: LuaExpr, inits:List[LuaExpr], conditions: List[LuaExpr],
                   loopExprs: List[LuaExpr], stmts: List[LuaStmt], namespacedName: Option[LuaNamespaceStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaIfThenElseStmt(cond: LuaExpr, stmts: List[LuaStmt], elseIfs: Option[List[LuaExpr]], elseifStmt: Option[List[LuaStmt]],
                   elseStmt: Option[List[LuaElseStmt]], attributes: LuaAttributes) extends LuaStmtWithBody
  final case class LuaElseStmt(stmts: List[LuaStmt], attributes: LuaAttributes) extends LuaStmtWithBody
  final case class LuaGotoStmt(label: LuaName, attributes: LuaAttributes)    extends LuaStmt
  final case class LuaLabelStmt(label: LuaName, attributes: LuaAttributes)    extends LuaStmt
  final case class LuaBreakStmt(attributes: LuaAttributes)    extends LuaStmt
  final case class LualocalAssignStmt(names: List[LuaExpr],init:List[LuaExpr], attributes: LuaAttributes)    extends LuaStmt
  final case class LuaAssignStmt(names: List[LuaExpr], init: List[LuaExpr], attributes: LuaAttributes)    extends LuaStmt
  final case class LuaNamespaceStmt(name: Option[LuaName], stmts: List[LuaStmt], attributes: LuaAttributes)    extends LuaStmtWithBody
  final case class LuaFileStmt(name: Option[LuaName],
                   stmts: List[LuaStmt],
                   classLikeType: String,
                   hasConstructor: Boolean,
                   attributes: LuaAttributes) extends LuaStmtWithBody
  final case class LuaParamStmt(name: LuaName,
                   paramType: Option[LuaNameExpr],
                   byRef: Boolean,
                   isVariadic: Boolean,
                   default: Option[LuaExpr],
                   flags: Int,
                   attributes: LuaAttributes) extends LuaStmt

  //读每种类型的Stat
  private def readStmt(json: Value): LuaStmt = {
    json("type").str match{
      case "Block" => readBlock(json)
      case "WhileDo" => readWhileDo(json)
      case "RepeatUntil" => readRepeatUntil(json)
      case "Return" => readReturn(json)
      case "FuncCallStat" => readFuncCallStat(json)
      case "FuncDef" => readFuncDef(json)
      case "LocalFuncDef" => readlocalFuncDef(json)
      case "GenericFor" => readGenericFor(json)
      case "NumericFor" => readNumericFor(json)
      case "IfThenElse" => readIfThenElse(json)
      case "Goto" => readGoto(json)
      case "Label" => readLabel(json)
      case "Break" => readBreak(json)
      case "LocalAssign" => readlocalAssign(json)
      case "Assign" => readAssign(json)
      case unhandled =>
        logger.error(s"Found unhandled stmt type: $unhandled")
        ???
    }
  }
//处理每个readstmt函数
  private def readBlock(json: Value): LuaBlockStmt = {
    val stmts = json("body").arr.toList.map(readStmt)
    LuaBlockStmt(stmts, LuaAttributes(json))
  }
  private def readWhileDo(json: Value): LuaWhileDoStmt ={
    val con = readExpr(json("condition"))
    val stmts = List(readStmt(json("body")))
    LuaWhileDoStmt(con,stmts,LuaAttributes(json))
  }
  private def readRepeatUntil(json: Value): LuaRepeatUntilStmt = {
    val con = readExpr(json("condition"))
//    val stmts = json("body").arr.toList.map(readStmt)
    val stmts = List(readStmt(json("body")))
    LuaRepeatUntilStmt(con, stmts, LuaAttributes(json))
  }
  private def readReturn(json: Value): LuaReturnStmt = {
    val expr = json("values").arr.toList.map(readExpr)
    LuaReturnStmt(expr, LuaAttributes(json))
  }
  private def readFuncCallStat(json: Value): LuaFuncCallStmt = {
    val expr = readExpr(json("funccall"))
    LuaFuncCallStmt(expr, LuaAttributes(json))
  }
  private def readFuncDef(json: Value): LuaFuncDefStmt = {
    val name = LuaNameExpr(readName(json("name").toString),LuaAttributes(json))
    //参数，处理为空的情况
    val params = json("body")("params") match{
      case arr: ujson.Arr if arr.value.isEmpty =>
        List(LuaParamStmt(LuaName("_",LuaAttributes(json)) ,None,false, false, None,0,LuaAttributes(json)))
      case arr => arr.arr.toList.map(readParam)
    }
    val stmts = json("body")("stmts").arr.toList.map(readStmt)
    val namespaceName = LuaName(name.toString,LuaAttributes(json))
    LuaFuncDefStmt(name, params, stmts, false, Some(namespaceName), LuaAttributes(json))
  }
  private def readlocalFuncDef(json: Value): LualocalFuncDefStmt = {
    val name = LuaNameExpr(readName(json("name").toString), LuaAttributes(json))
    //参数，处理为空的情况
    val params = json("body")("params") match {
      case arr: ujson.Arr if arr.value.isEmpty =>
        List(LuaParamStmt(LuaName("_", LuaAttributes(json)), None, false, false, None, 0, LuaAttributes(json)))
      case arr => arr.arr.toList.map(readParam)
    }
    val stmts = json("body")("stmts").arr.toList.map(readStmt)
    val namespaceName = LuaName(name.toString, LuaAttributes(json))
    LualocalFuncDefStmt(name, params, stmts, false, Some(namespaceName), LuaAttributes(json))
  }
  private def readGenericFor(json: Value): LuaGenericForStmt = {
    val names = json("names").arr.map(readExpr).toList
    val iterable: List[LuaExpr] = json("iterators").arr.toList.map(readExpr)
    val stmts = json("body").arr.toList.map(readStmt)
    val scope = LuaNamespaceStmt(Some(readName(json("scope").str)),stmts,LuaAttributes(json))
    LuaGenericForStmt(names ,iterable, stmts, Some(scope), LuaAttributes(json))
  }
  private def readNumericFor(json: Value): LuaNumericForStmt = {
    val name = LuaNameExpr(readName(json("name").toString),LuaAttributes(json))
    val init = json("init").arr.toList.map(readExpr)
    val limit = json("limit").arr.toList.map(readExpr)
    val step = json("step").arr.toList.map(readExpr)
    val stmts = json("body").arr.toList.map(readStmt)
    val scope = LuaNamespaceStmt(Some(readName(json("scope").str)),stmts,LuaAttributes(json))
    LuaNumericForStmt(name, init, limit, step, stmts, Some(scope), LuaAttributes(json))
  }
  private def readIfThenElse(json: Value): LuaIfThenElseStmt = {
    val cond = readExpr(json("ifCond"))
    val ifBody = json("ifBody").arr.toList.map(readStmt)
    val elifCond = json("elifCond").arr.toList.map(readExpr)
    val elifBody = json("elifBody").arr.toList.map(readStmt)
    val elseBody = json("elseBody").arr.toList.map(readElse)
    LuaIfThenElseStmt(cond, ifBody, Some(elifCond), Some(elifBody), Some(elseBody), LuaAttributes(json))
  }

  private def readElse(json: Value): LuaElseStmt = {
    val stmt = readStmt(json)
    LuaElseStmt(List(stmt), LuaAttributes(json))
  }
  private def readGoto(json: Value): LuaGotoStmt = {
    val label = readName(json("label").str)
    LuaGotoStmt(label, LuaAttributes(json))
  }
  private def readLabel(json: Value): LuaLabelStmt = {
    val label = readName(json("label").str)
    LuaLabelStmt(label, LuaAttributes(json))
  }
  private def readBreak(json: Value): LuaBreakStmt = {
    LuaBreakStmt(LuaAttributes(json))
  }
  private def readlocalAssign(json: Value): LualocalAssignStmt = {
    val vars: List[LuaExpr] = json("variables").arr.toList.map(readExpr)
    val init = json("init").arr.map(readExpr).toList
    LualocalAssignStmt(vars, init, LuaAttributes(json))
  }
  private def readAssign(json: Value): LuaAssignStmt = {
    val vars: List[LuaExpr] = json("variables").arr.map(readExpr).toList
    val init = json("init").arr.map(readExpr).toList
    LuaAssignStmt(vars, init, LuaAttributes(json))
  }
  private def readParam(json: Value): LuaParamStmt = {
    val attributes = LuaAttributes(json)
    val name = LuaName(json("name").toString,attributes)
    val isvararg = json("isvararg").toString match{
      case "true" => true
      case _ => false
    }
//    val stmts = json("body").arr.toList.map(readStmt)
    LuaParamStmt(name ,None,false,isvararg, None,0,attributes)
  }
//规定一个LuaName节点
  final case class LuaName(val value: String, val attributes: LuaAttributes) extends LuaStmt
  private def readName(str: String): LuaName = {
    LuaName(str, LuaAttributes(None,None,None,None))
  }
  //规定一个OpStmt节点
  final case class OpStmt(val value: String, val attributes: LuaAttributes) extends LuaStmt
  private def readOp(op: String): OpStmt = {
    //对操作符打表操作
    val OpMap: Map[String, String]=Map(
      "0" -> "=",
      "59" -> Operators.logicalOr,
      "60" -> Operators.logicalAnd,
      "61" -> Operators.notEquals,
      "62" -> Operators.greaterEqualsThan,
      "63" -> Operators.greaterThan,
      "20" -> Operators.logicalNot,
      "21" -> "<operator>.length",
      "19" -> "<operator>.unaryMinus",
      "22" -> "<operator>.concatenation",      //..
      "24" -> Operators.equals,
      "25" -> Operators.lessThan,
      "26" -> Operators.lessEqualsThan,
      "13" -> Operators.plus,
      "14" -> Operators.minus,
      "15" -> Operators.multiplication,
      "16" -> Operators.division,
      "17" -> Operators.modulo,       //%
      "18" -> Operators.xor
    )
    val opName:String = OpMap(op)
    OpStmt(opName, LuaAttributes(None,None,None,None))
  }
//定义Expr类型
  sealed trait LuaExpr extends LuaStmt
  final case class LuaNameExpr(name: LuaName, attributes: LuaAttributes) extends LuaExpr
  final case class LuaConstantExpr(name: LuaName, attributes: LuaAttributes) extends LuaExpr
  final case class LuaVarargsExpr(attributes: LuaAttributes) extends LuaExpr
  final case class LuaFieldExpr(table: LuaExpr,field: LuaName, attributes: LuaAttributes) extends LuaExpr
  final case class LuaListFieldExpr(rhs: LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaKeyedFieldExpr(index: LuaExpr,rhs: LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaNamedFieldExpr(name: LuaName,rhs: LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaIndexExpr(table: LuaExpr,index: LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaParensExpr(innerExp: LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaUnopExpr(operator: OpStmt, rhs:LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaBinopExpr(operator: OpStmt, left:LuaExpr, right:LuaExpr, attributes: LuaAttributes) extends LuaExpr
  final case class LuaMethodCallExpr(lhs:LuaExpr, name:LuaName,args:List[LuaExpr], attributes: LuaAttributes) extends LuaExpr
  final case class LuaFuncCallExpr( lhs:LuaExpr, args:List[LuaExpr], attributes: LuaAttributes) extends LuaExpr
  final case class LuaPrimaryExpr(expressionType: String, attributes: LuaAttributes) extends LuaExpr
  final case class LuaAnonFuncDefExpr(body: List[LuaStmt], attributes: LuaAttributes) extends LuaExpr
  final case class LuaTableConstructorExpr(body: List[LuaExpr], attributes: LuaAttributes) extends LuaExpr
  final case class LuaArrayDimFetchExpr(variable: LuaExpr, dimension: Option[LuaExpr], attributes: LuaAttributes)  extends LuaExpr
  final case class LuaUnknowExpr(attributes: LuaAttributes) extends LuaExpr
  //读每个Expr
  private def readExpr(json: Value): LuaExpr = {
  json("type").str match {
    case "VarExp"             => readNameExp(json)
    case "FieldExp"            => readFieldExp(json)
    case "KeyedField"     =>   readKeyedFieldExp(json)
    case "ListField"     =>   readListFieldExp(json)
    case "NamedField"     =>   readNamedFieldExp(json)
    case "IndexExp"            => readIndexExp(json)
    case "ParensExp"           => readParensExp(json)
    case "Constant" => readConstant(json)
    case "MethodCall" => readMethodCall(json)
    case "FuncCall"    => readFuncCAll(json)
    case "PrimaryExp"    => readPrimaryExp(json)
    case "VarargsExp"    => readVarargsExp(json)
    case "AnonFuncDef"    => readAnonFuncDef(json)
    case "BinaryOpExp"    => readBinopExp(json)
    case "UnOpExp"    => readUnopExp(json)
    case "TableConstructor"    => readTableConstructorExp(json)
    case "Unknownname" => readUnknownnameExp(json)
    case unhandled =>
      logger.error(s"Found unhandled expr type: $unhandled")
      ???
  }
}
//每个readEXpr相关内容
  private def readNameExp(json: Value): LuaNameExpr = {
    val name = readName(json("name").str)
    LuaNameExpr(name, LuaAttributes(json))
  }
  private def readConstant(json: Value): LuaConstantExpr = {
    val value = readName(json("value").str)
    LuaConstantExpr(value, LuaAttributes(json))
  }
  private def readFieldExp(json: Value): LuaFieldExpr = {
    val table = readExpr(json("table"))
    val field = readName(json("field").str)
    LuaFieldExpr(table, field, LuaAttributes(json))
  }
  //ListField
  private def readListFieldExp(json: Value): LuaListFieldExpr = {
    val rhs = readExpr(json("rhs"))
    LuaListFieldExpr(rhs, LuaAttributes(json))
  }

  //KeyedField
  private def readKeyedFieldExp(json: Value): LuaKeyedFieldExpr = {
    val index = readExpr(json("index"))
    val rhs = readExpr(json("rhs"))
    LuaKeyedFieldExpr(index, rhs, LuaAttributes(json))
  }

  //NamedField
  private def readNamedFieldExp(json: Value): LuaNamedFieldExpr = {
    val name =  readName(json("name").str)
    val rhs = readExpr(json("rhs"))
    LuaNamedFieldExpr(name, rhs, LuaAttributes(json))
  }
  private def readIndexExp(json: Value): LuaIndexExpr = {
    val table = readExpr(json("table"))
    val index = readExpr(json("index"))
    LuaIndexExpr(table, index, LuaAttributes(json))
  }
  private def readParensExp(json: Value): LuaParensExpr = {
    val exp = readExpr(json("innerExp"))
    LuaParensExpr(exp, LuaAttributes(json))
  }
  private def readUnopExp(json: Value): LuaUnopExpr = {
    val op = readOp(json("operator").toString)
    val rhs = readExpr(json("rhs"))
    LuaUnopExpr(op, rhs, LuaAttributes(json))
  }
  private def readBinopExp(json: Value): LuaBinopExpr = {
    val op = readOp(json("operator").toString)
    val left = readExpr(json("left"))
    val right = readExpr(json("right"))
    LuaBinopExpr(op, left, right, LuaAttributes(json))
  }
  private def readMethodCall(json: Value): LuaMethodCallExpr = {
    val lhs = readExpr(json("lhs"))
    val name = readName(json("name").str)
    val args = json("args").arr.toList.map(readExpr)
    LuaMethodCallExpr(lhs, name, args, LuaAttributes(json))
  }
  private def readFuncCAll(json: Value): LuaFuncCallExpr = {
    val lhs = readExpr(json("lhs"))
    val args = json("args").arr.toList.map(readExpr)
    LuaFuncCallExpr(lhs, args, LuaAttributes(json))
  }
  private def readVarargsExp(json: Value): LuaVarargsExpr = {
    LuaVarargsExpr(LuaAttributes(json))
  }
  private def readPrimaryExp(json: Value): LuaPrimaryExpr = {
    val exprtype = json("expressionType").toString
    LuaPrimaryExpr(exprtype ,LuaAttributes(json))
  }
  private def readAnonFuncDef(json: Value): LuaAnonFuncDefExpr = {
//    val params = json("body")("params").arr.toList.map(readExpr)
    val body = json("body")("stmts").arr.toList.map(readStmt)
    LuaAnonFuncDefExpr(body, LuaAttributes(json))
  }
  private def readTableConstructorExp(json: Value): LuaTableConstructorExpr = {
    val body = json("fields").arr.toList.map(readExpr)
    LuaTableConstructorExpr(body, LuaAttributes(json))
  }
  private def readUnknownnameExp(json: Value): LuaUnknowExpr = {
    LuaUnknowExpr(LuaAttributes(None,None,None,None))
  }
  //readFile,读入json值，输出luaFile格式，递归方法
  private def readFile(json: Value): LuaFile = {
    json match {
      case arr: Arr =>
        val children = arr.value.map(readStmt).toList
        LuaFile(children)
      case unhandled =>
        logger.error(s"Found unhandled type in readFile: ${unhandled.getClass} with value $unhandled")
        ???
    }
  }
  def fromJson(jsonInput: Value): LuaFile = {
    readFile(jsonInput)
  }
}

