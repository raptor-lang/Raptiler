package raptorscript.symbol

import raptorscript.ast.Node

class Symbol(val name: String) {
}

trait Type extends Scope {
  val name: String
}

class BuiltInTypeSymbol(override val name: String) extends Symbol(name) with Type {
  override def toString(): String = {
    s"$name: BLTINTYPE"
  }
}

class ClassSymbol(override val name: String) extends Symbol(name) with Type

class FunSymbol(
  _name: String,
  val retType: Type,
  _parentScope: Scope,
  val body: Node
) extends Symbol(_name) with Scope {
  override val parentScope: Option[Scope] = Some(_parentScope)
  val bodyScope: FunScope = new FunScope(this)

  override def toString(): String = {
    s"$name: ${retType}"
  }

}

class VarSymbol(override val name: String, val vType: Type) extends Symbol(name) with Type {
    override def toString(): String = {
    s"$name: ${vType}"
  }
}
