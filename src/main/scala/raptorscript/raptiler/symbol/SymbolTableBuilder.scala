package raptorscript.raptiler.symbol

import raptorscript.raptiler.ast.NodeVisitor
import raptorscript.raptiler.ast.Node
import raptorscript.raptiler.ast._

class SymbolTableBuilder(val symtab: SymbolTable) extends NodeVisitor {

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
      case n: Block => n.list.foreach(visit)
      case n: FunDecl => {
        val funSymbol = new FunSymbol(
          n.name,
          symtab.lookup(n.retType).get.asInstanceOf[Type],
          symtab.currentScope,
          n.body)
        symtab.define(funSymbol)
        symtab.push(funSymbol)
        n.args.list.foreach(visit)
        symtab.push(funSymbol.bodyScope)
        n.body.list.foreach(visit)
        symtab.pop()
        symtab.pop()
      }
      case n: VarDecl => {
        val typeSymbol = symtab.lookup(n.vType)
        val varSymbol = new VarSymbol(n.name, symtab.lookup(typeSymbol.get.name).get.asInstanceOf[Type])
        symtab.define(varSymbol)
      }
      case n: IfStatement => {
        visit(n.block)
        n.elseBlock.foreach(visit)
      }
      case _ ⇒
    }
  }
}
