package raptorscript.symbol

import raptorscript.{ast => a}
import raptorscript.interpreter.{Interpreter, RObject, RPrimitive}

object SInteger extends BuiltInTypeSymbol("INT") {
  override val parentScope: Option[Scope] = Some(GlobalScope)
}
