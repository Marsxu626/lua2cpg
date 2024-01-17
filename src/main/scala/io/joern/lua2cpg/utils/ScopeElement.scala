package io.joern.lua2cpg.utils

import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewNode, NewTypeDecl}

class ScopeElement private (val node: NewNode, scopeName: String) {

  def getClosureMethodName: String = {
    s"$scopeName"
  }
}

object ScopeElement {
  def apply(method: NewMethod): ScopeElement = {
    new ScopeElement(method, method.fullName)
  }

  def apply(typeDecl: NewTypeDecl): ScopeElement = {
    new ScopeElement(typeDecl, typeDecl.fullName)
  }

  def apply(namespace: NewNamespaceBlock): ScopeElement = {
    new ScopeElement(namespace, namespace.fullName)
  }

  def unapply(scopeElement: ScopeElement): Option[NewNode] = {
    Some(scopeElement.node)
  }
}
