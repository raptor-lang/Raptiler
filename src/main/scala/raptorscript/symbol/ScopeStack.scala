package raptorscript.symbol

import scala.collection.mutable.Stack


class ScopeStack {
  val globalScope = GlobalScope
  private var _currentScope: Scope = globalScope
  private var lastScopes: Stack[Scope] = Stack()

  define(SInteger)

  def currentScope = _currentScope

  def push(scope: Scope): Unit = _currentScope = scope

  def pop(): Unit = _currentScope = _currentScope.parentScope.get

  def define(symbol: Symbol): Unit = _currentScope.define(symbol)

  def lookup(name: String): Option[Symbol] = _currentScope.lookup(name)

  def enter(scope: Scope): Unit = lastScopes.push(scope)

  def exit(): Scope = lastScopes.pop()
}
