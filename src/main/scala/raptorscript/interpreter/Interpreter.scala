package raptorscript.interpreter

import scala.collection.mutable.{ ListBuffer, Stack }

import raptorscript.Parser
import raptorscript.symbol._
import raptorscript.ast._
import raptorscript.Tokens._
import raptorscript.memory.{MemoryStack, MemorySpace, FunctionSpace}

class Interpreter() {
  val scopeStack = new ScopeStack()
  val ast = ListBuffer[Node]()
  val ssb = new ScopeStackBuilder(scopeStack)
  val memStack = new MemoryStack()

  def interpret(parser: Parser): Any = {
    val xs = parser.program
    ssb visit xs
    ast += xs
    exec(xs)
  }

  def exec(node: Node): RObject = {
    node match {
      case n: VarDecl => {
        var v: RObject = RNull
        if (n.value.isDefined)
          v = exec(n.value.get)
          memStack.define(n.name, v)
          v
      }
      case n: VarAssign => {
        val v = exec(n.value)
        memStack.update(n.name, v)
        v
      }
      case n: VarAccess =>
        memStack.lookup(n.name).get
      case n: Block => n.list.map(exec).last
      case n: FunCall => {
        // HACKHACKEDYHACKHACK
        if (n.name == "print") {
          n.args.foreach(a => println(exec(a)))
          return RNull
        }
        val fs = scopeStack.lookup(n.name).get.asInstanceOf[FunSymbol]
        scopeStack.push(fs)
        memStack.push(new FunctionSpace(fs))
        if (fs.symbols.size != n.args.length)
          throw new raptorscript.RaptorError()
        for ((s, i) <- fs.symbols.values.zipWithIndex) {
          val v = exec(n.args(i))
          if (v.oType != s.asInstanceOf[VarSymbol].vType)
            throw new raptorscript.RaptorError("Trying to assign value incompatible type to variable")
          memStack.define(s.name, v)
        }
        scopeStack.push(fs.bodyScope)
        val result = exec(fs.body)
        scopeStack.pop()
        memStack.pop()
        scopeStack.pop()
        result
      }
      case n: LitteralInt =>
        RInteger(n.value)
      case n: LitteralFloat =>
        RFloat(n.value)
      case n: LitteralBool =>
        RBool(n.value)
      case n: LitteralString =>
        RString(n.value)
      case n: BinOp => {
        val left = exec(n.left)
        val right = exec(n.right)
        left match {
          case l: RPrimitive[Any] => l.binOp(n.token, right)
        }
      }
      case n: Program => {
        n.children.map(exec(_)).last
      }
      case n: IfStatement => {
        val cond = exec(n.cond)
        cond match {
          case c: RBool => {
            if (c.value)
              exec(n.block)
            else if (n.elseBlock.isDefined)
              exec(n.elseBlock.get)
            else
              RNull
          }
        }
      }
      case _ => RNull
    }
  }

}
