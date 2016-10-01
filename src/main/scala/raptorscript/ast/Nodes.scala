package raptorscript.ast

import raptorscript.{IToken, Token}

case class BinOp(
  left: IToken,
  token: IToken,
  right: IToken
) extends Node

case class UnaryOp(
  op: IToken,
  expr: IToken
) extends Node { val token = op}

case class Integer(
  token: Token[Int]
) extends Node { val value = token.value.get }

case class Float(
  token: Token[Float]
) extends Node { val value = token.value.get }

case class VarAccess(
  nameToken: Token[String]
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
  args: List[VarDecl],
  body: List[Node]
) extends Node {
  val name = nameToken.value.get
  val retType = typeToken.value.get
}

case class Program(
  children: List[Node]
) extends Node
