package raptorscript.symbol

import scala.collection.mutable.LinkedHashMap


trait Scope {
  val parentScope: Option[Scope] = None

  val symbols: LinkedHashMap[String, Symbol] = LinkedHashMap()

  def lookup(name: String): Option[Symbol] = symbols.get(name).orElse(parentScope.map(_.lookup(name).orNull))

  def define(symbol: Symbol): Unit = symbols.put(symbol.name, symbol)

  override def toString(): String = {
    val sbs: String = symbols.map(_._2.toString()).mkString(", ")
    s"${this.getClass.getCanonicalName()}: [$sbs]"
  }
}

class LocalScope(_parentScope: Scope) extends Scope {
  override val parentScope = Some(_parentScope)
}

class FunScope(val funSymbol: FunSymbol) extends Scope {
  override val parentScope = Some(funSymbol)
}

object GlobalScope extends Scope {
  override val parentScope: Option[Scope] = None
}
