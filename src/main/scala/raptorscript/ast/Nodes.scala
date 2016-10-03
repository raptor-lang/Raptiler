package raptorscript.ast

import raptorscript.{IToken, Token}
import raptorscript.interpreter.{Interpreter, RObject}

case class BinOp(
  left: Node,
  token: IToken,
  right: Node
) extends Node

case class UnaryOp(
  op: IToken,
  expr: Node
) extends Node { val token = op}

case class LitteralInt(
  token: Token[Int]
) extends Node { val value = token.value.get }

case class LitteralFloat(
  token: Token[scala.Float]
) extends Node { val value = token.value.get }

case class LitteralBool(
  token: Token[String],
  value: Boolean
) extends Node 

case class LitteralString(
  token: Token[String]
) extends Node { val value = token.value.get }

case class VarAccess(
  nameToken: Token[String]
) extends Node { val name = nameToken.value.get }

case class FunCall(
  nameToken: Token[String],
  args: List[Node]
) extends Node { val name = nameToken.value.get }

case class VarAssign(
  nameToken: Token[String],
  value: Node
) extends Node {
  val name = nameToken.value.get}

case class VarDecl(
  nameToken: Token[String],
  typeToken: Token[String],
  value: Option[Node] = None
) extends Node {
  val name = nameToken.value.get
  val vType = typeToken.value.get
}

case class FunDecl(
  nameToken: Token[String],
  typeToken: Token[String],
  args: FunVars,
  body: Block
) extends Node {
  val name = nameToken.value.get
  val retType = typeToken.value.get
}

case class IfStatement(cond: Node, block: Block, elseBlock: Option[Block]) extends Node

case class FunVars(list: List[VarDecl]) extends Node

case class Block(list: List[Node]) extends Node

case class Program(
  children: List[Node]
) extends Node

abstract case class BltInFun() extends Node {
  def exec(intrpr: Interpreter, self: RObject)
}
