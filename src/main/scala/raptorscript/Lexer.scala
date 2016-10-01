package raptorscript

import scala.collection.mutable.{ ArrayBuffer, Queue }
import scala.language.implicitConversions

class Lexer(text: String) {

  var pos: Int = 0
  var currentChar: Option[Char] = Some(text.charAt(pos))
  private var nextToken: Option[IToken] = None
  var currentToken: Option[IToken] = None
  val futureTokens = Queue[IToken]()

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

  private def lex(str: String, token: IToken): Unit = {
    if (shouldLex()) {
      if (text.slice(pos, pos + str.length()) == str) {
        lexAdvance(str.length())
        nextToken = Some(token)
      }
    }
  }

  private def lex(str: String, ttype: String): Unit = {
    lex(str, IToken(ttype, str))
  }

  private def lexNumber(): Unit = {
    if (shouldLex() && currentChar.forall(Character.isDigit)) {
      var str = ""
      while (currentChar.nonEmpty && currentChar.forall(c => Character.isDigit(c) || c == ".")) {
        str += currentChar.getOrElse("")
        lexAdvance()
      }
      if (str.contains("."))
        nextToken = Some(IToken(IToken.FLOAT, str.toFloat))
      else
        nextToken = Some(IToken(IToken.INT, str.toInt))
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
        nextToken = Some(IToken(IToken.KWORD, str))
      else
        nextToken = Some(IToken(IToken.NAME, str))
    }
  }

  private def getNextToken(): IToken = {
    if (futureTokens.length > 0) {
      val token = futureTokens(0)
      futureTokens.dequeue()
      return token
    }
    var token = IToken(IToken.EOF, "")
    while (currentChar == "") {
      if (currentChar.forall(Character.isSpace))
        lexAdvance()
      else {
        lexNumber()

        lex("+", IToken.PLUS)
        lex("-", IToken.MINUS)
        lex("*", IToken.ASTERISK)
        lex("/", IToken.SLASH)
        lex("(", IToken.LPAR)
        lex(")", IToken.RPAR)
        lex("{", IToken.LBRAC)
        lex("}", IToken.RBRAC)
        lex("[", IToken.LSQBRAC)
        lex("]", IToken.RSQBRAC)
        lex("<", IToken.LESS_THAN)
        lex(">", IToken.GREATER_THAN)
        lex("=", IToken.EQUALS)
        lex(";", IToken.EOF)
        lex(":", IToken.COLON)
        lex(",", IToken.COMMA)

        lexName()

        if (nextToken.isEmpty) {
          lexAdvance()
          throw new RaptorError()
        }
      }
    }
    return token
  }

  def advance(): IToken = {
    val tok = getNextToken()
    currentToken = Some(tok)
    return tok
  }

  def get(index: Int = 0): IToken = {
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

trait IToken {
  val ttype: String
  val value: Option[Any]
}

class Token[T](val ttype: String, val value: Option[T]) extends IToken {

  def this(ttype: String) = {
    this(ttype, None)
  }

  def this(ttype: String, value: T) {
    this(ttype, Some(value))
  }

  def equals(other: IToken): Boolean = {
    other.ttype == this.ttype
  }

  def apply(value: Any): IToken = {
    new Token(ttype, value)
  }
}

object Token {
  def apply[T](ttype: String, value: T) = {
    new Token(ttype, value)
  }

  def apply[T](ttype: String) = {
    new Token[T](ttype)
  }

  // TODO: Get rid of this shit
  implicit def iToken2StrToken(iToken: IToken): Token[String] = {
    new Token[String](iToken.ttype, iToken.value.get.toString())
  }

  // TODO: Get rid of this shit
  implicit def iToken2IntToken(iToken: IToken): Token[Int] = {
    new Token[Int](iToken.ttype, Integer.valueOf(iToken.value.get.toString()))
  }

  // TODO: Get rid of this shit. Seriously
  implicit def iToken2FloatToken(iToken: IToken): Token[Float] = {
    new Token[Float](iToken.ttype, java.lang.Float.valueOf(iToken.value.get.toString()))
  }
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

  val INT = Token("INT")
  val FLOAT = Token("FLOAT")
  val STRING = Token("STRING")
  val EOF = Token("EOF", "")

  val LPAR = Token("(", "(")
  val RPAR = Token(")", ")")
  val LBRAC = Token("{", "{")
  val RBRAC = Token("}", "}")
  val LSQBRAC = Token("[", "[")
  val RSQBRAC = Token("]", "]")
  val LESS_THAN = Token("<", "<")
  val GREATER_THAN = Token(">", ">")

  val KWORD = Token("KWORD")
  val NAME = Token("NAME")
}
