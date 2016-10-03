package raptorscript.symbol

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
      case n: FunVars => n.list.foreach(visit)
      case n: FunBody => n.list.foreach(visit)
      case n: FunDecl => {
        val funSymbol = new FunSymbol(
          n.name,
          stack.lookup(n.retType).get.asInstanceOf[Type],
          stack.currentScope,
          n.body)
        stack.define(funSymbol)
        stack.push(funSymbol)
        n.args.list.foreach(visit)
        stack.push(funSymbol.bodyScope)
        n.body.list.foreach(visit)
        stack.pop()
        stack.pop()
      }
      case n: VarDecl => {
        val typeSymbol = stack.lookup(n.vType)
        val varSymbol = new VarSymbol(n.name, stack.lookup(typeSymbol.get.name).get.asInstanceOf[Type])
        stack.define(varSymbol)
      }
      case _ ⇒
    }
  }
}
