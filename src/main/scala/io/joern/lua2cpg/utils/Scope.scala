package io.joern.lua2cpg.utils

import io.joern.lua2cpg.utils.ScopeElement
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.{Scope => X2CpgScope}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewNode, NewTypeDecl}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

import scala.collection.mutable

class Scope() extends X2CpgScope[String, NewNode, ScopeElement] {

  private var constAndStaticInits: List[mutable.ArrayBuffer[Ast]] = Nil
  private var fieldInits: List[mutable.ArrayBuffer[Ast]]          = Nil
  private val anonymousMethods                                    = mutable.ArrayBuffer[Ast]()

  def pushNewScope(scopeNode: NewNode): Unit = {
    scopeNode match {
      case method: NewMethod =>
        super.pushNewScope(ScopeElement(method))

      case typeDecl: NewTypeDecl =>
        constAndStaticInits = mutable.ArrayBuffer[Ast]() :: constAndStaticInits
        fieldInits = mutable.ArrayBuffer[Ast]() :: fieldInits
        super.pushNewScope(ScopeElement(typeDecl))

      case namespace: NewNamespaceBlock =>
        super.pushNewScope(ScopeElement(namespace))

      case invalid =>
        printf(s"pushNewScope called with invalid node $invalid. Ignoring!")
    }
  }

  override def popScope(): Option[ScopeElement] = {
    val scopeNode = super.popScope()

    scopeNode.map(_.node) match {
      case Some(_: NewTypeDecl) =>
        constAndStaticInits = constAndStaticInits.tail
        fieldInits = fieldInits.tail

      case _ => // Nothing to do here
    }
    scopeNode
  }

  override def addToScope(identifier: String, variable: NewNode): ScopeElement = {
    super.addToScope(identifier, variable)
  }

  def getEnclosingNamespaceNames: List[String] =
    stack.map(_.scopeNode.node).collect { case ns: NewNamespaceBlock => ns.name }.reverse

  def getEnclosingTypeDeclTypeName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.name)

  def getEnclosingTypeDeclTypeFullName: Option[String] =
    stack.map(_.scopeNode.node).collectFirst { case td: NewTypeDecl => td }.map(_.fullName)

  def addConstOrStaticInitToScope(ast: Ast): Unit = {
    addInitToScope(ast, constAndStaticInits)
  }
  def getConstAndStaticInits: List[Ast] = {
    getInits(constAndStaticInits)
  }

  def addFieldInitToScope(ast: Ast): Unit = {
    addInitToScope(ast, fieldInits)
  }

  def getFieldInits: List[Ast] = {
    getInits(fieldInits)
  }

  private def addInitToScope(ast: Ast, initList: List[mutable.ArrayBuffer[Ast]]): Unit = {
    // TODO This is unsafe to catch errors for now
    initList.head.addOne(ast)
  }

  private def getInits(initList: List[mutable.ArrayBuffer[Ast]]): List[Ast] = {
    // TODO This is unsafe to catch errors for now
    val ret = initList.head.toList
    // These ASTs should only be added once to avoid aliasing issues.
    initList.head.clear()
    ret
  }
}
