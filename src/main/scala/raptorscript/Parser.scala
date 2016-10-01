package raptorscript
import raptorscript.Tokens._
import raptorscript.Token

class Parser(lexer: Lexer) {

  private def eat(): Token = {
    val result = lexer.get()
    lexer.advance()
    return result
  }

  private def eat(ttype: String): Token = {
    if (next(ttype))
      return eat()
    else
      throw new RaptorError()
  }

  private def eat(ttype: Token): Token = {
    if (next(ttype))
      return eat()
    else
      throw new RaptorError()
  }

  private def next(ttype: String*): Boolean = {
    for ((t,i) <- ttype.zipWithIndex)
      if (lexer.get(i).ttype != t)
        return false
    return true
  }

  private def next(ttype: Token*): Boolean = {
    return next(ttype.map(_.ttype):_*)
  }

  private def next = lexer.get(0)

  def factor = {
    var un: Option[Token] = next match {
      case PLUS | MINUS => Some(eat)
      case _ => None
    }
    var node = next match {
      case LPAR => parens
      case NAME => access
      case NUMBER => a.Num(eat)
    }
    if (un.nonEmpty)
      a.UnaryOp(un, node)
    else
      node
  }

  def access = a.Access(eat NAME)

  def parens = {
    eat(LPAR)
    val t = expr
    eat(RPAR)
    t
  } 

  def addend = {
    var node = factor
    while (next == ASTERISK || next == SLASH) {
      val op = eat
      node = a.BinOp(node, op, factor)
    }
    node
  }

  def number = {
    var node = addend
    while (next == PLUS || next == MINUS) {
      val op = eat
      node = a.BinOp(node, op, addend)
    }
    node
  }

  def expr = number

  def varAssign = {
    val name = eat NAME
    eat EQUALS
    val value = expr
    a.VarAssign(name, value)
  }

  def varDecl = {
    eat(Token(KWORD, "var"))
    val name = eat NAME

  }

}
