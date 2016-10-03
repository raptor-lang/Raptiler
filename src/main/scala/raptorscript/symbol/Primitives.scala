package raptorscript.symbol

import raptorscript.{ast => a}
import raptorscript.interpreter.{Interpreter, RObject, RPrimitive}

object SInteger extends BuiltInTypeSymbol("INT") {
  override val parentScope: Option[Scope] = Some(GlobalScope)
}

object SFloat extends BuiltInTypeSymbol("FLOAT") {
  override val parentScope: Option[Scope] = Some(GlobalScope)
}

object SBool extends BuiltInTypeSymbol("BOOL") {
  override val parentScope: Option[Scope] = Some(GlobalScope)
}

object SString extends BuiltInTypeSymbol("STRING") {
  override val parentScope: Option[Scope] = Some(GlobalScope)
}
