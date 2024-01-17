package io.joern.lua2cpg.astcreation

import org.luaj.vm2.ast._
import overflowdb.BatchedUpdate
import io.joern.lua2cpg.utils.Scope
import io.joern.lua2cpg.astcreation.AstCreator.{TypeConstants,operatorSymbols}
import io.joern.x2cpg.Ast.storeInDiffGraph
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.Defines.{StaticInitMethodName, UnresolvedNamespace, UnresolvedSignature}
import io.joern.x2cpg.utils.AstPropertiesUtil.RootProperties
import io.joern.x2cpg.{Ast, AstCreatorBase, AstNodeBuilder, ValidationMode}
import io.joern.lua2cpg.parser.Domain.*
import io.joern.lua2cpg.utils.ArrayIndexTracker
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.slf4j.LoggerFactory


class AstCreator(filename: String, LuaFile: LuaFile)(implicit withSchemaValidation: ValidationMode)
  extends AstCreatorBase(filename)
    with AstNodeBuilder[LuaNode, AstCreator] {
  private val globalNamespace = globalNamespaceBlock()
  private val scope           = new Scope()
  private val logger          = LoggerFactory.getLogger(AstCreator.getClass)
  override def createAst(): BatchedUpdate.DiffGraphBuilder = {
    val ast = astForLuaFile(LuaFile)
    storeInDiffGraph(ast, diffGraph)
    diffGraph
  }

  private def flattenGlobalNamespaceStmt(stmt: LuaStmt): List[LuaStmt] = {
    stmt match {
      case namespace: LuaNamespaceStmt if namespace.name.isEmpty =>
        namespace.stmts

      case _ => stmt :: Nil
    }
  }

  private def globalMethodDeclStmt(file: LuaFile, bodyStmts: List[LuaStmt]): LuaFuncDefStmt = {
    LuaFuncDefStmt(
      name = LuaNameExpr(LuaName(NamespaceTraversal.globalNamespaceName,file.attributes),file.attributes),
      params = Nil,
      stmts = bodyStmts,
      returnByRef = false,
      namespacedName = None,
      attributes = file.attributes
    )
  }
  private def astForLuaStmt(stmt: LuaFileStmt): Ast = {
    stmt.name match {
      case None => Ast()
      case Some(name) => astForNamedLua(stmt, name)
    }
  }

  private def astForNamedLua(stmt: LuaFileStmt, name: LuaName): Ast = {
    val inheritsFrom =  Seq.empty
    val code = ""
    val fullName = globalNamespace.fullName
    val typeDecl = typeDeclNode(stmt, name.value, fullName, filename, code, inherits = inheritsFrom)
    scope.pushNewScope(typeDecl)
    val bodyStmts = astsForLuaBody(stmt, stmt.stmts)
    scope.popScope()
    Ast(typeDecl).withChildren(bodyStmts)
  }

  private def astsForLuaBody(LuaLike: LuaStmt, bodyStmts: List[LuaStmt]): List[Ast] = {

    val otherBodyStmts = bodyStmts.flatMap {
      case stmt => astForStmt(stmt)
      case _ => Seq(Ast())
    }
    otherBodyStmts
  }

  private def astForLuaFile(file: LuaFile): Ast = {
    scope.pushNewScope(globalNamespace)
    val globalMethodStmts= file.children.flatMap(flattenGlobalNamespaceStmt)
    val globalMethodStmt = globalMethodDeclStmt(file, globalMethodStmts)
    val globalTypeDeclStmt = LuaFileStmt(
      name = Some(LuaName(globalNamespace.name, file.attributes)),
      stmts = List(globalMethodStmt),
      classLikeType="",
      hasConstructor = false,
      attributes = file.attributes
    )
    val globalTypeDeclAst = astForLuaStmt(globalTypeDeclStmt)
    scope.popScope()
    Ast(globalNamespace).withChild(globalTypeDeclAst)
//    Ast()
  }

  private def astForStmt(stmt: LuaStmt): List[Ast] = {
    stmt match {
      case localAssign: LualocalAssignStmt => astForlocalAssignStmt(localAssign) :: Nil
      case assign: LuaAssignStmt => astForAssignStmt(assign) :: Nil
      case block: LuaBlockStmt => astForBlockStmt(block) :: Nil
      case funcDef: LuaFuncDefStmt => astForFuncDefStmt(funcDef):: Nil
      case funcDef: LualocalFuncDefStmt => astForlocalFuncDefStmt(funcDef):: Nil
      case funcCall: LuaFuncCallStmt => astForFuncCallStmt(funcCall):: Nil
      //控制结构
      case breakStmt: LuaBreakStmt => astForBreakStmt(breakStmt) :: Nil
      case whileDoStmt: LuaWhileDoStmt => astForWhileDoStmt(whileDoStmt) :: Nil
      case repeatUntilStmt: LuaRepeatUntilStmt => astForRepeatUntilStmt(repeatUntilStmt) :: Nil
      case genericForStmt: LuaGenericForStmt => astForGenericForStmt(genericForStmt) :: Nil
      case numericForStmt: LuaNumericForStmt => astForNumericForStmt(numericForStmt) :: Nil
      case ifThenElseStmt: LuaIfThenElseStmt => astForIfThenElseStmt(ifThenElseStmt) :: Nil
      case gotoStmt: LuaGotoStmt => astForGotoStmt(gotoStmt) :: Nil
      case labelStmt: LuaLabelStmt => astForLabelStmt(labelStmt) :: Nil
      case returnstmt:LuaReturnStmt => astForReturnStmt(returnstmt) :: Nil
      case unhandled =>
        logger.error(s"Unhandled stmt $unhandled in $filename")
        ???
    }
  }
  private def astForFuncCallStmt(funcCall: LuaFuncCallStmt): Ast = {
    val nameAst = astForExpr(funcCall.funccall)
    nameAst
  }
  private def astForlocalAssignStmt(localAssign: LualocalAssignStmt): Ast = {
    val vars = localAssign.names.map(astForExpr)
    val init = localAssign.init.map(astForExpr)
    val args= vars ++ init
    val callNode = newOperatorCallNode("<operation>.Assign", "", line = line(localAssign),column= column(localAssign))
    callAst(callNode,args)
  }
  private def astForAssignStmt(assign: LuaAssignStmt): Ast = {
    val vars = assign.names.map(astForExpr)
    val init = assign.init.map(astForExpr)
    val args = vars ++ init
    val callNode = newOperatorCallNode("<operation>.Assign", "", line = line(assign), column = column(assign))
    callAst(callNode, args)
  }

  private def astForBlockStmt(block: LuaBlockStmt): Ast = {
    val bodyBlock = blockNode(block)
    val bodyStmtAsts = block.stmts.flatMap(astForStmt)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }
  //处理参数节点
  private def astForParam(param: LuaParamStmt, index: Int): Ast = {
    val evaluationStrategy =
      if (param.byRef)
        EvaluationStrategies.BY_REFERENCE
      else
        EvaluationStrategies.BY_VALUE
    val typeFullName = param.paramType.map(_.name.value.toString).getOrElse(TypeConstants.Any)
    val byRefCodePrefix = if (param.byRef) "&" else ""
    val code = s"$byRefCodePrefix$$${param.name}"
    val paramNode = parameterInNode(param, param.name.value, code, index, param.isVariadic, evaluationStrategy, typeFullName)
    scope.addToScope(param.name.value, paramNode)
    Ast(paramNode)
  }
  //Stmt-函数定义
  private def astForFuncDefStmt(funcDef: LuaFuncDefStmt):Ast = {
    val methodName :String = funcDef.name match{
      case name: LuaNameExpr => name.name.value
      case _ => "anonyFunction"
    }
    val parameters = funcDef.params.zipWithIndex.map { case (param, idx) =>
      astForParam(param, idx + 1)
    }
    val signature = s"$UnresolvedSignature(${funcDef.params.size})"
    val methodCode = s"$methodName(${parameters.map(_.rootCodeOrEmpty).mkString(",")})"
    val method = methodNode(funcDef, methodName, methodCode, "fullName", Some(signature), filename)
    scope.pushNewScope(method)
    val returnType = TypeConstants.Any
    val methodBodyStmts= funcDef.stmts.flatMap(astForStmt)
    val methodBody = blockAst(blockNode(funcDef), methodBodyStmts)
    val methodReturn = newMethodReturnNode(returnType, line = line(funcDef), column = column((funcDef)))
    scope.popScope()
    methodAstWithAnnotations(method, parameters, methodBody, methodReturn)
  }

  private def astForlocalFuncDefStmt(funcDef: LualocalFuncDefStmt): Ast = {
    val methodName: String = funcDef.name match {
      case name: LuaNameExpr => name.name.value
      case _ => "anonyFunction"
    }
    val parameters = funcDef.params.zipWithIndex.map { case (param, idx) =>
      astForParam(param, idx + 1)
    }
    val signature = s"$UnresolvedSignature(${funcDef.params.size})"
    val methodCode = s"function $methodName(${parameters.map(_.rootCodeOrEmpty).mkString(",")})"
    val method = methodNode(funcDef, methodName, methodCode, "fullName", Some(signature), filename)
    scope.pushNewScope(method)
    val returnType = TypeConstants.Any
    val methodBodyStmts = funcDef.stmts.flatMap(astForStmt)
    val methodBody = blockAst(blockNode(funcDef), methodBodyStmts)
    val methodReturn = newMethodReturnNode(returnType, line = line(funcDef), column = column((funcDef)))
    scope.popScope()
    methodAstWithAnnotations(method, parameters, methodBody, methodReturn)
  }
  private def astForConstantExpr(constant: LuaConstantExpr): Ast = {
    val name: String = constant.name.value
    val callNode = literalNode(constant, name, "")
    Ast(callNode)
  }
  private def intToLiteralAst(num: Int): Ast = {
    Ast(NewLiteral().code(num.toString).typeFullName(TypeConstants.Int))
  }
//控制结构-BreakStmt
  private def astForBreakStmt(breakStmt: LuaBreakStmt): Ast = {
    val code = "break"
    val breakNode = controlStructureNode(breakStmt, ControlStructureTypes.BREAK, code)
    val argument = List(intToLiteralAst(0))
    controlStructureAst(breakNode, None, argument)
  }
//控制结构-WhileDoStmt
  private def astForWhileDoStmt(whileDoStmt: LuaWhileDoStmt): Ast = {
    val condition = astForExpr(whileDoStmt.cond)
    val lineNumber = line(whileDoStmt)
    val code       = s"while (${condition.rootCodeOrEmpty})"
    val body = stmtBodyBlockAst(whileDoStmt)
    whileAst(Option(condition), List(body), Option(code), lineNumber)
  }
//批量处理stats
  private def stmtBodyBlockAst(stmt: LuaStmtWithBody): Ast = {
    val bodyBlock = blockNode(stmt)
    val bodyStmtAsts = stmt.stmts.flatMap(astForStmt)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }
//控制结构-RepeatUntilStmt
  private def astForRepeatUntilStmt(repeatUntilStmt: LuaRepeatUntilStmt): Ast = {
    val condition = astForExpr(repeatUntilStmt.cond)
    val lineNumber = line(repeatUntilStmt)
    val code = s"repeatUntil ${condition.rootCodeOrEmpty}"
    val body = stmtBodyBlockAst(repeatUntilStmt)
    whileAst(Option(condition), List(body), Option(code), lineNumber)
  }
//控制结构-astForNumericForStmt
private def astForNumericForStmt(stmt: LuaNumericForStmt): Ast = {
  val lineNumber = line(stmt)
  val nameAst = List(astForExpr(stmt.name))
  val initAsts = stmt.inits.map(astForExpr)
  val conditionAsts = stmt.conditions.map(astForExpr)
  val loopExprAsts = stmt.loopExprs.map(astForExpr)
  val bodyAst = stmtBodyBlockAst(stmt)

  val initCode = initAsts.map(_.rootCodeOrEmpty).mkString(",")
  val conditionCode = conditionAsts.map(_.rootCodeOrEmpty).mkString(",")
  val loopExprCode = loopExprAsts.map(_.rootCodeOrEmpty).mkString(",")
  val numericForCode = s"for ($initCode;$conditionCode;$loopExprCode)"

  val ForNode = controlStructureNode(stmt, ControlStructureTypes.FOR, numericForCode)
  forAst(ForNode, nameAst, initAsts, conditionAsts, loopExprAsts, bodyAst)
  
}
//控制结构-GenericForStmt
  private def astForGenericForStmt(stmt: LuaGenericForStmt): Ast = {
  val bodyBlock = blockNode(stmt)
  val iteratorAst    = stmt.iterExpr.map(astForExpr)
  val iterAst =  Ast(bodyBlock).withChildren(iteratorAst)
  val pairAst = stmt.names match {
    case List(key, value) =>
      val keyAst = astForExpr(key)
      val valueAst = astForExpr(value)
      val code = s"${keyAst.rootCodeOrEmpty} => ${valueAst.rootCodeOrEmpty}"
      val callNode = newOperatorCallNode("<operator>.doubleArrow", code, line = line(stmt))
      callAst(callNode, keyAst :: valueAst :: Nil)
    case _ => Ast()
  }
  val bodyAst = stmtBodyBlockAst(stmt)
  val genericForCode = s"for ${iteratorAst.map(_.rootCodeOrEmpty).mkString(",")}"
  val genericForNode = controlStructureNode(stmt, ControlStructureTypes.FOR, genericForCode)
  Ast(genericForNode)
    .withChild(iterAst)
    .withChild(pairAst)
    .withChild(bodyAst)
}

  private def getItemAssignAstForForeach(
    stmt: LuaGenericForStmt,
    assignItemTargetAst: Ast,
    iteratorIdentifier: NewIdentifier): Ast = {
    val iteratorIdentifierAst = Ast(iteratorIdentifier)
    val currentCallSignature = s"$UnresolvedSignature(0)"
    val currentCallCode = s"${iteratorIdentifierAst.rootCodeOrEmpty}->current()"
    val currentCallNode = callNode(
      stmt,
      currentCallCode,
      "current",
      "Iterator.current",
      DispatchTypes.DYNAMIC_DISPATCH,
      Some(currentCallSignature),
      Some(TypeConstants.Any)
    );
    val currentCallAst = callAst(currentCallNode, base = Option(iteratorIdentifierAst))

    val valueAst =   currentCallAst

    simpleAssignAst(assignItemTargetAst, valueAst, line(stmt))
  }

  private def simpleAssignAst(target: Ast, source: Ast, lineNo: Option[Integer]): Ast = {
    val code = s"${target.rootCodeOrEmpty} = ${source.rootCodeOrEmpty}"
    val callNode = newOperatorCallNode(Operators.assignment, code, line = lineNo)
    callAst(callNode, target :: source :: Nil)
  }

  //控制结构-IfThenElseStmt
  private def astForIfThenElseStmt(stmt: LuaIfThenElseStmt): Ast = {
    val condition = astForExpr(stmt.cond)
    val thenAst = stmtBodyBlockAst(stmt)
    var elseifs = stmt.elseIfs
    var elseifstmt = stmt.elseifStmt
    val (elif, elifs) = elseifs match {
      case Some(list) if list.nonEmpty => (Some(list.head), Some(list.tail))
      case _ => (None, None)
    }
    val (elifstmt, elifstmts) = elseifstmt match {
      case Some(list) if list.nonEmpty => (Some(list.head), Some(list.tail))
      case _ => (None, None)
    }
    val elseAst: List[Ast] = (elifs, elifstmts) match {
      case (Some(elifcon), Some(elifstmts)) =>
       (elif, elifstmt) match {
          case (Some(ifcon), Some(ifstmt)) =>
            val newIfStmt = LuaIfThenElseStmt(ifcon, List(ifstmt), Some(elifcon), Some(elifstmts), stmt.elseStmt, ifcon.attributes)
            val wrappingBlock = blockNode(ifcon)
            val wrappedAst = Ast(wrappingBlock).withChild(astForIfThenElseStmt(newIfStmt)) :: Nil
            wrappedAst
          case _ =>
            List(Ast())
        }
      case _ =>
        stmt.elseStmt match {
          case Some(elseStmt) =>
            elseStmt.map(stmtBodyBlockAst)
          case _ =>
            List(Ast())
        }
    }
    val conditionCode = condition.rootCodeOrEmpty
    val ifNode = controlStructureNode(stmt, ControlStructureTypes.IF, s"($conditionCode)")
    controlStructureAst(ifNode, Option(condition), thenAst :: elseAst)
  }
//控制结构-GotoStmt
  private def astForGotoStmt(stmt: LuaGotoStmt): Ast = {
    val label = stmt.label.value
    val code = s"goto $label"
    val gotoNode = controlStructureNode(stmt, ControlStructureTypes.GOTO, code)
    val jumpLabel = NewJumpLabel()
    .name(label)
    .code(label)
    .lineNumber(line(stmt))
    controlStructureAst(gotoNode, condition = None, children = Ast(jumpLabel) :: Nil)
  }
//控制结构-LabelStmt
  private def astForLabelStmt(stmt: LuaLabelStmt): Ast = {
    val label = stmt.label.value
    val jumpTarget = NewJumpTarget()
      .name(label)
      .code(label)
      .lineNumber(line(stmt))
    Ast(jumpTarget)
  }

//ReturnStmt
  private def astForReturnStmt(stmt:LuaReturnStmt): Ast = {
    val maybeExprAst = stmt.expr.map(astForExpr)
    val code = s"return ${maybeExprAst.map(_.rootCodeOrEmpty)}"
    val node = returnNode(stmt, code)
    returnAst(node, maybeExprAst.toList)
  }
//对Expr进行转换
  private def astForExpr(expr: LuaExpr): Ast = {
    expr match {
      case constant: LuaConstantExpr => astForConstantExpr(constant)
      case luaName: LuaNameExpr => astForluaNameExpr(luaName)
      case unaryOp: LuaUnopExpr => astForUnaryOp(unaryOp)
      case binaryOp: LuaBinopExpr => astForBinOpExpr(binaryOp)
      case funcCall: LuaFuncCallExpr => astForFuncCallExpr(funcCall)
      case anonFunc: LuaAnonFuncDefExpr => astForAnonFuncDefExpr(anonFunc)
      case funcCall: LuaMethodCallExpr => astForMethodCallExpr(funcCall)
      case listField: LuaListFieldExpr => astForListFieldExpr(listField)
      case keyField: LuaKeyedFieldExpr => astForKeyedFieldExpr(keyField)
      case nameField: LuaNamedFieldExpr => astForNamedFieldExpr(nameField)
      case table: LuaTableConstructorExpr =>   astForTableConstructorExpr(table)
      case index: LuaIndexExpr  => astForIndexExpr(index)
      case field: LuaFieldExpr => astForFieldExpr(field)
      case unknow: LuaUnknowExpr => astForUnknowExpr(unknow)
      case varsrgs:LuaVarargsExpr => astForVarargsExpr(varsrgs)
        case parens:LuaParensExpr  => astForParensExpr(parens)
      case null =>
        logger.warn("expr was null")
        ???
      case other => throw new NotImplementedError(s"unexpected expression '$other' of type ${other.getClass}")
    }
  }
//Expr-Name
  private def astForluaNameExpr(luaName: LuaNameExpr): Ast = {
    val name = luaName.name.value
    val callNode = identifierNode(luaName, name, "", name)
    //添加变量的数据依赖
    scope.lookupVariable(name).foreach { declaringNode =>
      diffGraph.addEdge(callNode, declaringNode, EdgeTypes.REF)
    }
    Ast(callNode)
  }
//Expr-unaryop
  private def astForUnaryOp(unaryOp: LuaUnopExpr): Ast = {
    val exprAst = astForExpr(unaryOp.rhs)
    val symbol = operatorSymbols.getOrElse(unaryOp.operator.value, unaryOp.operator.value)
    val code = s"$symbol${exprAst.rootCodeOrEmpty}"
    val callNode = newOperatorCallNode(unaryOp.operator.value, code, line = line(unaryOp))
    callAst(callNode, exprAst :: Nil)
}
//Expr-Binop
  private def astForBinOpExpr(binOp: LuaBinopExpr): Ast = {
    val leftAst = astForExpr(binOp.left)
    val rightAst = astForExpr(binOp.right)
//    val symbol = operatorSymbols.getOrElse(binOp.operator, binOp.operator)
    val code = s"${leftAst.rootCodeOrEmpty} ${binOp.operator} ${rightAst.rootCodeOrEmpty}"
    val callNode = newOperatorCallNode(binOp.operator.value, code, line = line(binOp))
    callAst(callNode, List(leftAst, rightAst))
  }
//Expr-FuncCall
  private def astForFuncCallExpr(funcCall:LuaFuncCallExpr): Ast = {
    val name = funcCall.lhs match{
      case luaName: LuaNameExpr =>
        val name = luaName.name.value
        val callNode = newOperatorCallNode(name, "", line = line(funcCall), column = column(funcCall))
        callNode
      case _ =>
        val callNode = newOperatorCallNode("anonymous func", "", line = line(funcCall), column = column(funcCall))
        callNode
    }
    val args = funcCall.args.map(astForExpr)
    callAst(name, args)
  }
//暂时识别方法，外边对象的连接还没实现
    private def astForMethodCallExpr(funcCall: LuaMethodCallExpr): Ast = {
    val objectname =  funcCall.lhs match {
      case luaName: LuaNameExpr =>
        val name = luaName.name.value
        name
      case _ =>
        None
    }
    val funcname = funcCall.name.value
    val callNode = newOperatorCallNode (funcname, "", line = line (funcCall), column = column (funcCall) )
    callNode
    val args = funcCall.args.map(astForExpr)
    callAst(callNode, args)
  }

//Expr-AnonFuncDef
  private def astForAnonFuncDefExpr(funcCall:LuaAnonFuncDefExpr): Ast = {
    val callNode = newOperatorCallNode("AnonFuncDef", "", line = line(funcCall), column = column(funcCall))
//    val args = funcCall.args.map(astForExpr)
    val bodyBlock = blockNode(funcCall)
    val bodyStmtAsts = funcCall.body.flatMap(astForStmt)
    val body = List(Ast(bodyBlock).withChildren(bodyStmtAsts))
    callAst(callNode, body)
  }

//Expr-tableConstructor
  private def astForTableConstructorExpr(tableConstructor: LuaTableConstructorExpr): Ast = {
    val bodyBlock = blockNode(tableConstructor)
    val bodyStmtAsts = tableConstructor.body.map(astForExpr)
    Ast(bodyBlock).withChildren(bodyStmtAsts)
  }

//Expr-tableConstructor
  private def astForListFieldExpr(listField:LuaListFieldExpr): Ast = {
    val idxTracker = new ArrayIndexTracker
    val tmpIdentifier = getTmpIdentifier(listField, Some(TypeConstants.Array))
    val itemAssignments = List(assignForArrayItem(listField.rhs, tmpIdentifier.name, idxTracker))
    val arrayBlock = blockNode(listField)
    Ast(arrayBlock)
      .withChildren(itemAssignments)
      .withChild(Ast(tmpIdentifier))
  }

  private def astForKeyedFieldExpr(keyField: LuaKeyedFieldExpr): Ast = {
    val idxTracker = new ArrayIndexTracker
    val tmpIdentifier = getTmpIdentifier(keyField, Some(TypeConstants.Array))
    val itemAssignments = List(assignForArray2Item(keyField, tmpIdentifier.name, idxTracker))
    val arrayBlock = blockNode(keyField)
    Ast(arrayBlock)
      .withChildren(itemAssignments)
      .withChild(Ast(tmpIdentifier))
  }

  private def astForNamedFieldExpr(nameField: LuaNamedFieldExpr): Ast = {
    val idxTracker = new ArrayIndexTracker
    val tmpIdentifier = getTmpIdentifier(nameField, Some(TypeConstants.Array))
    val itemAssignments = List(assignForArray3Item(nameField, tmpIdentifier.name, idxTracker))
    val arrayBlock = blockNode(nameField)
    Ast(arrayBlock)
      .withChildren(itemAssignments)
      .withChild(Ast(tmpIdentifier))
  }

  private val tmpKeyPool      = new IntervalKeyPool(first = 0, last = Long.MaxValue)
  private def getNewTmpName(prefix: String = "tmp"): String = s"$prefix${tmpKeyPool.next.toString}"
  private def getTmpIdentifier(originNode: LuaNode,
                               maybeTypeFullName: Option[String],
                               prefix: String = ""
                              ): NewIdentifier = {
    val name = s"$prefix${getNewTmpName()}"
    val typeFullName = maybeTypeFullName.getOrElse(TypeConstants.Any)
    identifierNode(originNode, name, s"$$$name", typeFullName)
  }

  private def assignForArrayItem(rhs: LuaExpr, name: String, idxTracker: ArrayIndexTracker): Ast = {
    val variable = LuaNameExpr(LuaName(name, rhs.attributes), rhs.attributes)
//    val dimFetchNode = LuaArrayDimFetchExpr(variable, None, rhs.attributes)
//    val dimFetchAst = astForArrayDimFetchExpr(dimFetchNode)
    val valueAst = astForExpr(rhs)
    val assignCode = s"${valueAst.rootCodeOrEmpty}"
    val assignNode = newOperatorCallNode(Operators.assignment, assignCode, line = line(rhs))
    callAst(assignNode, valueAst :: Nil)
  }

  private def assignForArray2Item(rhs: LuaKeyedFieldExpr, name: String, idxTracker: ArrayIndexTracker): Ast = {
    val variable = LuaNameExpr(LuaName(name, rhs.attributes), rhs.attributes)
    val dimension = rhs.index
//    val dimFetchNode = LuaArrayDimFetchExpr(variable, None, rhs.attributes)
    val dimFetchAst = astForExpr(dimension)
    val valueAst = astForExpr(rhs.rhs)
    val assignCode = s"${valueAst.rootCodeOrEmpty}"
    val assignNode = newOperatorCallNode(Operators.assignment, assignCode, line = line(rhs))
    callAst(assignNode, dimFetchAst :: valueAst :: Nil)
  }

  private def assignForArray3Item(rhs: LuaNamedFieldExpr, name: String, idxTracker: ArrayIndexTracker): Ast = {
    val variable = LuaNameExpr(LuaName(name, rhs.attributes), rhs.attributes)
    val dimension = LuaNameExpr(rhs.name,rhs.attributes)
    val dimFetchAst = astForExpr(dimension)
    val valueAst = astForExpr(rhs.rhs)
    val assignCode = s"${valueAst.rootCodeOrEmpty}"
    val assignNode = newOperatorCallNode(Operators.assignment, assignCode, line = line(rhs))
    callAst(assignNode, dimFetchAst :: valueAst :: Nil)
  }

  private def astForArrayDimFetchExpr(expr: LuaArrayDimFetchExpr): Ast = {
    val variableAst = astForExpr(expr.variable)
    val variableCode = variableAst.rootCodeOrEmpty

    expr.dimension match {
      case Some(dimension) =>
        val dimensionAst = astForExpr(dimension)
        val code = s"$variableCode[${dimensionAst.rootCodeOrEmpty}]"
        val accessNode = newOperatorCallNode(Operators.indexAccess, code, line = line(expr))
        callAst(accessNode, variableAst :: dimensionAst :: Nil)

      case None =>
        val errorPosition = s"${variableCode}:${line(expr).getOrElse("")}:${filename}"
        logger.error(s"ArrayDimFetchExpr without dimensions should be handled in assignment: ${errorPosition}")
        Ast()
    }
  }

//Expr-Index
  private def astForIndexExpr(index: LuaIndexExpr): Ast = {
    val ind = astForExpr(index.index)
    val table = astForExpr(index.table)
    val assignNode = newOperatorCallNode("<operator>.ArrayIdx", "indexcode", line = line(index))
    callAst(assignNode, ind :: table :: Nil)
  }

//Expr-field
  private def astForFieldExpr(field: LuaFieldExpr): Ast = {
    val nameAst = astForluaNameExpr(LuaNameExpr(field.field,field.attributes))
    val fieldAst = astForExpr(field.table)
    val assignNode = newOperatorCallNode("Field", "fieldcode", line = line(field))
    callAst(assignNode, nameAst :: fieldAst :: Nil)
  }
  private def astForUnknowExpr(unkown: LuaUnknowExpr) : Ast = {
    Ast()
  }
  private def astForVarargsExpr(varsrgs:LuaVarargsExpr): Ast = {
    Ast()
  }

  private def astForParensExpr(parens:LuaParensExpr): Ast = {
    val Ast = astForExpr(parens.innerExp)
    Ast
  }
  protected def line(luaNode: LuaNode): Option[Integer] = luaNode.attributes.beginLine
  protected def column(luaNode: LuaNode): Option[Integer] = luaNode.attributes.beginColumn
  protected def lineEnd(luaNode: LuaNode): Option[Integer] = luaNode.attributes.endLine
  protected def columnEnd(luaNode: LuaNode): Option[Integer] = luaNode.attributes.endColumn
  protected def code(luaNode: LuaNode): String               = ""
}

object AstCreator {
  object TypeConstants {
    val String: String = "string"
    val Int: String = "int"
    val Float: String = "float"
    val Bool: String = "bool"
    val Void: String = "void"
    val Any: String = "ANY"
    val Array: String = "array"
    val NullType: String = "null"
  }

  val operatorSymbols: Map[String, String] = Map(
    Operators.equals -> "=",
    Operators.logicalOr -> "|",
    Operators.logicalAnd -> "&",
    Operators.notEquals -> "!=",
    Operators.greaterEqualsThan -> ">=",
    Operators.greaterThan -> ">",
    Operators.logicalNot -> "!",
    "<operator>.length" -> "<operator>.length",
    "<operator>.unaryMinus" -> "<operator>.unaryMinus",
    "<operator>.concatenation" -> "<operator>.concatenation",
    Operators.equals -> "==",
    Operators.lessThan -> "<",
    Operators.lessEqualsThan -> "<=",
    Operators.plus -> "+",
    Operators.minus -> "-",
    Operators.multiplication -> "*",
    Operators.division -> "/",
    Operators.modulo -> "%",
    Operators.xor -> "^"
  )
}
