package raptorscript.scope

import raptorscript.ast.NodeVisitor
import raptorscript.ast.Node
import raptorscript.ast._
import raptorscript.RaptorError

class ScopeStackBuilder(val stack: ScopeStack) extends NodeVisitor {

  def visit(node: Node): Unit = {
    node match {
      case n: BinOp => {
        visit(n.left)
        visit(n.right)
      }
      case n: UnaryOp => {
        visit(n.expr)
      }
      case n: Program => {
        n.children.foreach(visit)
      }
      case n: FunDecl => {
        val funSymbol = new FunSymbol(
          n.name,
          n.retType,
          stack.currentScope)
        stack.define(funSymbol)
        stack.push(funSymbol)
        n.args.foreach(visit)
        stack.push(funSymbol.bodyScope)
        n.body.foreach(visit)
        stack.pop()
        stack.pop()
      }
      case n: VarDecl => {
        val typeSymbol = stack.lookup(n.vType)
        val varSymbol = new VarSymbol(n.name, typeSymbol.get.name)
        stack.define(varSymbol)
      }
      case _ ⇒
    }
  }
}
