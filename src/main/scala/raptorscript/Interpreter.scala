package raptorscript

import raptorscript.scope.{ScopeStack, ScopeStackBuilder}
import raptorscript.ast.Node
import scala.collection.mutable.ListBuffer

class Interpreter() {
  val parser = new Parser(new Lexer(" "))
  val scopeStack = new ScopeStack()
  val ast = ListBuffer[Node]()
  val ssb = new ScopeStackBuilder(scopeStack)

  def interpret(): String = {
    val xs = parser.program
    ssb visit xs
    ast += xs
    scopeStack.globalScope.toString()
  }

  def interpret(text: String): String = {
    parser.lexer.text += text
    interpret()
  }
}
