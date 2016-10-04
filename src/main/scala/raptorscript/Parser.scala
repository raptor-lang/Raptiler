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

  private def eat(ttype: IToken*): IToken = {
    if (ttype.contains(next))
      eat()
    else
      throw new RaptorError(s"Unexpected token $next. Expected $ttype")
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

  def access: Node = {
    val name = eat(NAME)
    if (next == LPAR) {
      eat(LPAR)
      val args = ListBuffer[Node]()
      while (next != RPAR) {
        args += expr()
        if (next != RPAR)
          eat(COMMA)
      }
      eat(RPAR)
      a.FunCall(name, args.toList)
    }
    else
      a.VarAccess(name)
  }

  def parens: Node = {
    eat(LPAR)
    val t = expr()
    eat(RPAR)
    t
  } 

  def expr(opLevl: Int = 0): Node = {
    var node: Node = null
    node = next match {
      case LPAR => parens
      case NAME => access
      case LBRAC => block
      case KWORD => {
        next.asInstanceOf[Token[String]].value.get match {
          case "true" => a.LitteralBool(eat, true)
          case "false" => a.LitteralBool(eat, false)
        }
      }
      case STRING => a.LitteralString(eat)
      case INT => a.LitteralInt(eat())
      case FLOAT => a.LitteralFloat(eat(FLOAT))
    }

    // Operator precedence. Highest orders on top
    if (opLevl >= -2) 
      while (next == ASTERISK || next == SLASH)
        node = a.BinOp(node, eat, expr(-3))
    if (opLevl >= -1) 
      while (next == PLUS || next == MINUS)
        node = a.BinOp(node, eat, expr(-2))
    if (opLevl >= 0) 
      if (List(EQEQ, NEQ, LESS_THAN, GREATER_THAN, LTEQ, GTEQ).contains(next))
        node = a.BinOp(node, eat, expr(-1))
    node
  }

  def varAssign: Node = {
    val name = eat(NAME)
    eat(EQUALS)
    val value = expr()
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
      xs += funArg
      if (next != RPAR)
        eat(COMMA)
    }
    eat(RPAR)
    eat(COLON)
    val typ = eat(NAME)
    eat(EQUALS)
    val blck = block
    a.FunDecl(name, typ, a.FunVars(xs.toList), blck)
  }

  def funArg: a.VarDecl = {
    val name = eat(NAME)
    eat(COLON)
    val typeName = eat(NAME)
    a.VarDecl(name, typeName, None)
  }

  def ifStatement: Node = {
    eat(KWORD)
    val ex = expr()
    val blck = block
    var els: Option[a.Block] = None
    if (beat(KWORD("else"))) {
      els = Some(block)
    }
    new a.IfStatement(ex, blck, els)
  }

  def block: a.Block = {
    val statements = ListBuffer[Node]()
    if (beat(LBRAC)) {
      while (next != RBRAC) {
        statements += statement
      }
      eat(RBRAC)
    } else
      statements += statement
    a.Block(statements.toList)
  }

  def statement: Node = {
    var node: Node = null
    if (next(NAME, EQUALS)) {
      node = varAssign
    } else if (next == KWORD) {
      node = next.asInstanceOf[Token[String]].value.get match {
        case "var" => varDecl
        case "fun" => funDecl
        case "if" => ifStatement
        case _ => expr()
      }
    } else
      node = expr()
    beat(SEMICOLON)
    node
  }

  def program: a.Program = {
    val statements = ListBuffer[Node]()
    while (next != EOF) {
      statements += statement
    }
    a.Program(statements.toList)
  }

}
