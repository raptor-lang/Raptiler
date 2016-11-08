package raptorscript.raptiler.symbol

import scala.collection.mutable.Stack

class SymbolTable {
  val globalScope = GlobalScope
  private var _currentScope: Scope = globalScope

  define(SInteger)
  define(SBool)
  define(SFloat)
  define(SString)

  def currentScope = _currentScope

  def push(scope: Scope): Unit = _currentScope = scope

  def pop(): Unit = _currentScope = _currentScope.parentScope.get

  def define(symbol: Symbol): Unit = _currentScope.define(symbol)

  def lookup(name: String): Option[Symbol] = _currentScope.lookup(name)
}


class ScopeStack {
  val symtab = new SymbolTable()
  private var stack: Stack[Scope] = Stack(GlobalScope)

  def currentScope = stack.top

  def push(scope: Scope): Unit = stack.push(scope)

  def pop(): Unit = stack.pop()

  def define(symbol: Symbol): Unit = currentScope.define(symbol)

  def lookup(name: String): Option[Symbol] = currentScope.lookup(name)
}
