package raptorscript.scope

class Symbol(val name: String, val sType: Option[String] = None)

trait Type

class BuiltInTypeSymbol(val name: String) extends Symbol(name) with Type

class ClassSymbol(val name: String) extends Symbol(name) with Type

class FunSymbol(
  val name: String,
  val retType: String,
  _parentScope: Scope
) extends Symbol(name, Some(retType)) with Type with Scope {
  val parentScope: Option[Scope] = Some(_parentScope)
  val bodyScope: FunScope = new FunScope(this)
}

class VarSymbol(val name: String, val vType: String) extends Symbol(name, Some(vType)) with Type
