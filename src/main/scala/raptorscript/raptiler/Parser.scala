package raptorscript.raptiler

import raptorscript.raptiler.Tokens._
import raptorscript.raptiler.{ast => a}
import raptorscript.raptiler.base.ParserBase
import a.Node
import scala.collection.mutable.ListBuffer

class Parser(lexer: Lexer) extends ParserBase(lexer) {

  /**
    * The starting point of the program
    */
  def program: a.Program = {
    val statements = ListBuffer[Node]()
    while (next != EOF) {
      statements += statement
    }
    a.Program(statements.toList)
  }

  /**
    * A statement. Can be optionally terminated by a semicolon
    * statement: (varDecl | funDecl | expr) SEMICOLON?
    */
  def statement: Node = {
    var node: Node = null
    if (next == KWORD) {
      node = next.asInstanceOf[Token[String]].value.get match {
        case "var" => varDecl
        case "fun" => funDecl
        case _ => expr
      }
    } else
      node = expr
    beat(SEMICOLON)
    node
  }

  /**
    * Shortcut to the expression
    */
  def expr: Node = expr()

  /**
    * Expressions, the most important RaptorScript construct.
    * Practically everything is an expression.
    */
  def expr(opLevl: Int = 0): Node = {
    var node: Node = null
    node = next match {
      case LPAR => parens
      case NAME => {
        if (next(NAME, EQUALS))
          varAssign
        else if (next(NAME, LPAR))
          funcCall
        else
          access
      }
      case LBRAC => block
      case KWORD => {
        next.asInstanceOf[Token[String]].value.get match {
          case "true" => a.LitteralBool(eat, true)
          case "false" => a.LitteralBool(eat, false)
          case "if" => ifStatement
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

  /**
    * Access a variable or field
    * access: NAME
    */
  def access: Node = {
    val name = eat(NAME)
    a.VarAccess(name)
  }

  /**
    * Call a function
    * funcCall: name LPAR (expr COMMA)* expr? RPAR
    */
  def funcCall: Node = {
    val name = eat(NAME)
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

  /**
    * An expression in parentheses
    * parens: LPAR expr RPAR
    */
  def parens: Node = {
    eat(LPAR)
    val t = expr
    eat(RPAR)
    t
  }

  /**
    * Assign a value to a variable
    * varAssign: NAME EQUALS expr
    */
  def varAssign: Node = {
    val name = eat(NAME)
    eat(EQUALS)
    val value = expr()
    a.VarAssign(name, value)
  }

  def typeName: IToken = {
    eat(NAME)
  }

  /**
    * A variable declaration, with or without assignment
    * varDecl: "var" NAME COLON typeName (EQUALS expr)?
    */
  def varDecl: a.VarDecl = {
    eat(KWORD("var"))
    val name = eat(NAME)
    eat(COLON)
    val typ = typeName
    var value: Option[Node] = None
    if (beat(EQUALS))
      value = Some(expr())
    a.VarDecl(name, typ, value)
  }

  /**
    * A function declaration.
    * funDecl: "fun" NAME LPAR (funArgDef COMMA)* funArg? RPAR COLON typeName EQUALS expr
    */
  def funDecl: a.FunDecl = {
    eat(KWORD("fun"))
    val name = eat(NAME)
    eat(LPAR)
    val xs = ListBuffer[a.VarDecl]()
    while (next != RPAR) {
      xs += funArgDef
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

  /**
    * A definition of a function argument
    * funArgDef: NAME COLON typeName
    */
  def funArgDef: a.VarDecl = {
    val name = eat(NAME)
    eat(COLON)
    val typ = typeName
    a.VarDecl(name, typ, None)
  }

  /**
    * An if statement, with optional else block
    * ifStatement: "if" expr expr ("else" expr)?
    */
  def ifStatement: Node = {
    eat(KWORD)
    val ex = expr
    val blck = expr
    var els: Option[Node] = None
    if (beat(KWORD("else"))) {
      els = Some(expr)
    }
    new a.IfStatement(ex, blck, els)
  }

  /**
    * A code block
    * LBRAC statement* RBRAC
    */
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


}
