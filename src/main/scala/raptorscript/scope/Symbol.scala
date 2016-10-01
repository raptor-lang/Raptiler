package raptorscript.scope

class Symbol(val name: String, val sType: Option[String] = None)

trait Type

class BuiltInTypeSymbol(override val name: String) extends Symbol(name) with Type

class ClassSymbol(override val name: String) extends Symbol(name) with Type

class FunSymbol(
  _name: String,
  val retType: String,
  _parentScope: Scope
) extends Symbol(_name, Some(retType)) with Type with Scope {
  override val parentScope: Option[Scope] = Some(_parentScope)
  val bodyScope: FunScope = new FunScope(this)
}

class VarSymbol(override val name: String, val vType: String) extends Symbol(name, Some(vType)) with Type
