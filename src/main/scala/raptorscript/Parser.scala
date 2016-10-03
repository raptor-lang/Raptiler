package raptorscript
import raptorscript.Tokens._
import raptorscript.{ast => a}
import a.Node
import scala.collection.mutable.ListBuffer

class Parser(val lexer: Lexer) {

  private def eat(): IToken = {
    val result = lexer.get()
    lexer.advance()
    result
  }

  private def eat(ttype: IToken): IToken = {
    if (next(ttype))
      eat()
    else
      throw new RaptorError()
  }

  /**
    * Eats the token if it is one of the suplied types.
    * @param tokens The first matching token will be eaten.
    * @return true if a token was eaten
    */
  private def beat(tokens: IToken*): Boolean = {
    for (token <- tokens) {
      if (next == token) {
        eat()
        return true
      }
    }
    false
  }

  private def next(tokens: IToken*): Boolean = {
    for ((t,i) <- tokens.zipWithIndex)
      try {
        if (lexer.get(i) != t)
          return false
      } catch {
        case e: NoSuchElementException =>
          return false
      }
    true
  }

  private def next: IToken = lexer.get(0)

  def factor: Node = {
    val n = next
    val un: Option[IToken] = n match {
      case PLUS | MINUS => Some(eat)
      case _ => None
    }
    val node: Node = n match {
      case LPAR => parens
      case NAME => access
      case INT => a.Integer(eat())
      case FLOAT => a.Float(eat())
      case _ ⇒ throw new RaptorError()
    }
    if (un.nonEmpty)
      a.UnaryOp(un.get, node)
    else
      node
  }

  def access: Node = {
    val name = eat(NAME)
    if (next == LPAR) {
      eat(LPAR)
      val args = ListBuffer[Node]()
      while (next != RPAR) {
        args += expr
      }
      eat(RPAR)
      a.FunCall(name, args.toList)
    }
    else
      a.VarAccess(name)
  }

  def parens: Node = {
    eat(LPAR)
    val t = expr
    eat(RPAR)
    t
  } 

  def addend: Node = {
    var node = factor
    while (next == ASTERISK || next == SLASH)
      node = a.BinOp(node, eat, factor)
    node
  }

  def number: Node = {
    var node = addend
    while (next == PLUS || next == MINUS) 
      node = a.BinOp(node, eat, addend)
    node
  }

  def expr: Node = number

  def varAssign: Node = {
    val name = eat(NAME)
    eat(EQUALS)
    val value = expr
    a.VarAssign(name, value)
  }

  def varDecl: a.VarDecl = {
    eat(KWORD("var"))
    val name = eat(NAME)
    eat(COLON)
    val typeName = eat(NAME)
    var value: Option[Node] = None
    if (beat(EQUALS))
      value = Some(statement)
    a.VarDecl(name, typeName, value)
  }

  def funDecl: a.FunDecl = {
    eat(KWORD("fun"))
    val name = eat(NAME)
    eat(LPAR)
    val xs = ListBuffer[a.VarDecl]()
    while (next != RPAR) {
      xs += varDecl
    }
    eat(RPAR)
    eat(COLON)
    val typ = eat(NAME)
    eat(LBRAC)
    val xs2 = ListBuffer[Node]()
    while (next != RBRAC) {
      xs2 += statement
    }
    eat(RBRAC)
    a.FunDecl(name, typ, a.FunVars(xs.toList), new a.FunBody(xs2.toList))
  }

  def statement: Node = {
    var node: Node = null
    if (next(NAME, EQUALS))
      node = varAssign
    else
    if (next == KWORD)
      node = next.asInstanceOf[Token[String]].value.get match {
        case "var" => varDecl
        case "fun" => funDecl
      }
    else
      node = expr
    node
  }

  def program: a.Program = {
    val statements = ListBuffer[Node]()
    var run = true
    while (next != EOF) {
      statements += statement
    }
    a.Program(statements.toList)
  }

}
