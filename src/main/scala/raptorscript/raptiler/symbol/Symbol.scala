package raptorscript.raptiler.symbol

import raptorscript.raptiler.ast.Node

class Symbol(val name: String, val typ: Type) {
  var index = 0
  var scope: Scope = null
}

trait Type extends Scope {
  val name: String
}

object TypeType extends Type {
  val name = "TYPE"
}

class BuiltInTypeSymbol(override val name: String) extends Symbol(name, TypeType) with Type {
  override def toString(): String = {
    s"$name: BLTINTYPE"
  }
}

class ClassSymbol(override val name: String) extends Symbol(name, TypeType) with Type

class FunSymbol(
  _name: String,
  val retType: Type,
  _parentScope: Scope,
  val body: Node
) extends Symbol(_name, retType) with Scope {
  override val parentScope: Option[Scope] = Some(_parentScope)
  val bodyScope: FunScope = new FunScope(this)

  override def toString(): String = {
    s"$name: ${retType}"
  }

}

class VarSymbol(override val name: String, val vType: Type) extends Symbol(name, vType) with Type {
    override def toString(): String = {
    s"$name: ${vType}"
  }
}
