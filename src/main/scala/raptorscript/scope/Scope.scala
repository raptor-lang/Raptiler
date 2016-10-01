package raptorscript.scope

import scala.collection.mutable.LinkedHashMap


trait Scope {
  val parentScope: Option[Scope] = None

  private val symbols: LinkedHashMap[String, Symbol] = LinkedHashMap()

  def lookup(name: String): Option[Symbol] = symbols.get(name).orElse(parentScope.map(_.lookup(name).orNull))

  def define(symbol: Symbol): Unit = symbols.put(symbol.name, symbol)
}

class LocalScope(_parentScope: Scope) extends Scope {
  override val parentScope = Some(_parentScope)
}

class FunScope(val funSymbol: FunSymbol) extends Scope {
  override val parentScope = Some(funSymbol)
}

class GlobalScope extends Scope {
  override val parentScope: Option[Scope] = None
}
