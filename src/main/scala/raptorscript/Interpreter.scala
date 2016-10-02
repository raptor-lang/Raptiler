package raptorscript

import scala.collection.mutable.{ ListBuffer, Stack }

import raptorscript.scope.{ScopeStack, ScopeStackBuilder}
import raptorscript.ast._
import raptorscript.Tokens._
import raptorscript.memory.{MemorySpace, FunctionSpace}

class Interpreter() {
  val scopeStack = new ScopeStack()
  val ast = ListBuffer[Node]()
  val ssb = new ScopeStackBuilder(scopeStack)
  val globalSpace = new MemorySpace("GLOBALS")
  val memStack = new Stack[MemorySpace]()
  memStack.push(globalSpace)

  def interpret(parser: Parser): Any = {
    val xs = parser.program
    ssb visit xs
    ast += xs
    exec(xs)
  }

  def exec(node: Node): Int = {
    node match {
      case n: VarDecl => {
        if (n.value.isDefined) {
          val v = exec(n.value.get)
          memStack.top.define(n.name, v)
          return v
        }
        0
      }
      case n: VarAssign => {
        val v = exec(n.value)
        memStack.top.define(n.name, v)
        v
      }
      case n: VarAccess =>
        memStack.top.get(n.name).getOrElse(0).toString.toInt
      case n: Integer =>
        n.value
      case n: BinOp => {
        n.token match {
          case PLUS =>
            exec(n.left).toString().toInt + exec(n.right).toString().toInt
          case MINUS =>
            exec(n.left).toString().toInt - exec(n.right).toString().toInt
          case ASTERISK =>
            exec(n.left).toString().toInt * exec(n.right).toString().toInt
          case SLASH =>
            exec(n.left).toString().toInt / exec(n.right).toString().toInt
        }
      }
      case n: Program => {
        n.children.map(exec(_)).last
      }
      case _ => 0
    }
  }

}
