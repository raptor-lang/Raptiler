package raptorscript.symbol

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
