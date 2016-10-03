package raptorscript.interpreter

import scala.collection.mutable.{ ListBuffer, Stack }

import raptorscript.Parser
import raptorscript.symbol._
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

  def exec(node: Node): RObject = {
    node match {
      case n: VarDecl => {
        if (n.value.isDefined) {
          val v = exec(n.value.get)
          memStack.top.define(n.name, v)
          return v
        }
        RNull
      }
      case n: VarAssign => {
        val v = exec(n.value)
        memStack.top.define(n.name, v)
        v
      }
      case n: VarAccess =>
        memStack.top.get(n.name).get
      case n: FunBody => n.list.map(exec).last
      case n: FunCall => {
        val fs = scopeStack.lookup(n.name).get.asInstanceOf[FunSymbol]
        scopeStack.push(fs)
        memStack.push(new FunctionSpace(fs))
        if (fs.symbols.size != n.args.length)
          throw new raptorscript.RaptorError()
        for ((s, i) <- fs.symbols.values.zipWithIndex) {
          val v = exec(n.args(i))
          if (v.oType != s.asInstanceOf[VarSymbol].vType)
            throw new raptorscript.RaptorError()
          memStack.top.define(s.name, v)
        }
        scopeStack.push(fs.bodyScope)
        val result = exec(fs.body)
        scopeStack.pop()
        memStack.pop()
        scopeStack.pop()
        result
      }
      case n: Integer =>
        new RPrimitive[Int](SInteger, n.value)
      case n: BinOp => {
        val left = exec(n.left).asInstanceOf[RPrimitive[Int]].value
        val right = exec(n.right).asInstanceOf[RPrimitive[Int]].value
        new RPrimitive[Int](SInteger, n.token match {
          case PLUS =>
            left + right
          case MINUS =>
            left - right
          case ASTERISK =>
            left * right
          case SLASH =>
            left / right
        })
      }
      case n: Program => {
        n.children.map(exec(_)).last
      }
      case _ => RNull
    }
  }

}
