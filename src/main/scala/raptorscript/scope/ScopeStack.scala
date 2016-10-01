package raptorscript.scope

class ScopeStack {
  val globalScope = new GlobalScope()
  private var _currentScope: Scope = globalScope

  def currentScope = _currentScope

  def push(scope: Scope): Unit = _currentScope = scope

  def pop(): Unit = _currentScope = _currentScope.parentScope.get

  def define(symbol: Symbol): Unit = _currentScope.define(symbol)

  def lookup(name: String): Option[Symbol] = _currentScope.lookup(name)
}
