package raptorscript

import scala.collection.mutable.{ ArrayBuffer, Queue }

class Lexer(text: String) {

  var pos: Int = 0
  var currentChar: Option[Char] = Some(text.charAt(pos))
  private var nextToken: Option[Token] = None
  var currentToken: Option[Token] = None
  val futureTokens = Queue[Token]()

  private def lexAdvance(n: Int = 1): Unit = {
    pos += n
    if (pos > text.length() - 1)
      currentChar = None
    else
      currentChar = Some(text.charAt(pos))
  }

  private def shouldLex(): Boolean = {
    nextToken == None && currentChar != None
  }

  private def lex(str: String, token: Token): Unit = {
    if (shouldLex()) {
      if (text.slice(pos, pos + str.length()) == str) {
        lexAdvance(str.length())
        nextToken = Some(token)
      }
    }
  }

  private def lex(str: String, ttype: String): Unit = {
    lex(str, Token(ttype, str))
  }

  private def lexNumber(): Unit = {
    if (shouldLex() && currentChar.forall(Character.isDigit)) {
      var str = ""
      while (currentChar.nonEmpty && currentChar.forall(c => Character.isDigit(c) || c == ".")) {
        str += currentChar.getOrElse("")
        lexAdvance()
      }
      if (str.contains("."))
        nextToken = Some(Token(Token.FLOAT, str.toFloat))
      else
        nextToken = Some(Token(Token.INT, str.toInt))
    }
  }

  private def lexName(): Unit = {
    if (shouldLex() && currentChar.forall(c => Character.isLetter(c) || c == "_")) {
      var str = ""
      while (currentChar.nonEmpty && currentChar.forall(c => Character.isLetter(c) || c == "_")) {
        str += currentChar.getOrElse("")
        lexAdvance()
      }
      if (Lexer.KEYWORDS.contains(str))
        nextToken = Some(Token(Token.KWORD, str))
      else
        nextToken = Some(Token(Token.NAME, str))
    }
  }

  private def getNextToken(): Token = {
    if (futureTokens.length > 0) {
      val token = futureTokens(0)
      futureTokens.dequeue()
      return token
    }
    var token = Token(Token.EOF, "")
    while (currentChar == "") {
      if (currentChar.forall(Character.isSpace))
        lexAdvance()
      else {
        lexNumber()

        lex("+", Token.PLUS)
        lex("-", Token.MINUS)
        lex("*", Token.ASTERISK)
        lex("/", Token.SLASH)
        lex("(", Token.LPAR)
        lex(")", Token.RPAR)
        lex("{", Token.LBRAC)
        lex("}", Token.RBRAC)
        lex("[", Token.LSQBRAC)
        lex("]", Token.RSQBRAC)
        lex("<", Token.LESS_THAN)
        lex(">", Token.GREATER_THAN)
        lex("=", Token.EQUALS)
        lex(";", Token.EOF)
        lex(":", Token.COLON)
        lex(",", Token.COMMA)

        lexName()

        if (nextToken.isEmpty) {
          lexAdvance()
          throw new RaptorError()
        }
      }
    }
    return token
  }

  def advance(): Token = {
    val tok = getNextToken()
    currentToken = Some(tok)
    return tok
  }

  def get(index: Int = 0): Token = {
    if (index == 0)
      return currentToken.get
    if (futureTokens.length < index)
      for (i <- 0 to (index - futureTokens.length)) {
        futureTokens.enqueue(getNextToken())
      }
    return futureTokens.get(index).get
  }
}

object Lexer {
  val KEYWORDS = List(
    "var"
  )
}

case class Token(ttype: String, value: Any) {

}

object Tokens {
  val PLUS = Token("+", "+")
  val MINUS = Token("-", "-")
  val ASTERISK = Token("*", "*")
  val SLASH = Token("/", "/")
  val EQUALS = Token("=", "=")
  val COLON = Token(":", ":")
  val COMMA = Token(",", ",")
  val NOT = Token("NOT", "!")
  val AND = Token("AND", "&&")
  val OR = Token("OR", "||")

  val INT = "INT"
  val FLOAT = "FLOAT"
  val STRING = "STRING"
  val EOF = "EOF"

  val LPAR = "("
  val RPAR = ")"
  val LBRAC = "{"
  val RBRAC = "}"
  val LSQBRAC = "["
  val RSQBRAC = "]"
  val LESS_THAN = "<"
  val GREATER_THAN = ">"

  val KWORD = "KWORD"
  val NAME = "NAME"
}
