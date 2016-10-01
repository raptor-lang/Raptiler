package raptorscript
import raptorscript.Tokens._
import raptorscript.{ast => a}

class Parser(lexer: Lexer) {

  private def eat(): IToken = {
    val result = lexer.get()
    lexer.advance()
    return result
  }

  private def eat(ttype: IToken): IToken = {
    if (next(ttype))
      return eat()
    else
      throw new RaptorError()
  }

  /**
    * Eats the token if it is one of the suplied types.
    * @param tokens The first matching token will be eaten.
    * @returns true if a token was eaten
    */
  private def beat(tokens: IToken*): Boolean = {
    for (token <- tokens) {
      if (next == token) {
        eat()
        return true
      }
    }
    return false
  }

  private def next(tokens: IToken*): Boolean = {
    for ((t,i) <- tokens.zipWithIndex)
      if (lexer.get(i) != t)
        return false
    return true
  }

  private def next = lexer.get(0)

  def factor = {
    var un: Option[IToken] = next match {
      case PLUS | MINUS => Some(eat)
      case _ => None
    }
    var node = next match {
      case LPAR => parens
      case NAME => access
      case INT => a.Int(eat)
      case FLOAT => a.Float(eat)
    }
    if (un.nonEmpty)
      a.UnaryOp(un, node)
    else
      node
  }

  def access = a.Access(eat(NAME))

  def parens = {
    eat(LPAR)
    val t = expr
    eat(RPAR)
    t
  } 

  def addend = {
    var node = factor
    while (beat(ASTERISK, SLASH)) {
      node = a.BinOp(node, op, factor)
    }
    node
  }

  def number = {
    var node = addend
    while (beat(PLUS, MINUS)) {
      node = a.BinOp(node, op, addend)
    }
    node
  }

  def expr = number

  def varAssign = {
    val name = eat(NAME)
    eat(EQUALS)
    val value = expr
    a.VarAssign(name, value)
  }

  def varDecl = {
    eat(KWORD("var"))
    val name = eat(NAME)
    eat(COLON)
    val typeName = eat(NAME)
    var value: Option[Node] = None
    if (beat(EQUALS))
      value = Some(statement)
    a.VarDecl(name, typeName, value)
  }

  def statement = {
    val kvar = KWORD("var")
    val node = next match {
      case NAME | EQUALS => varAssign
      case kvar => varDecl
      case _ => expr
    }
    node
  }

  def program = {
    val statements = ListBuffer[Node]
    var run = true
    while (next != EOF) {
      statements += statement
    }
    a.Program(List(statements))
  }

}
