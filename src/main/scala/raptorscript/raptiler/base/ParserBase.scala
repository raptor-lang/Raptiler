package raptorscript.raptiler.base

import raptorscript.raptiler.{Lexer, IToken, RaptorError}

class ParserBase(val lexer: Lexer) {


  protected def eat(): IToken = {
    val result = lexer.get()
    lexer.advance()
    result
  }

  protected def beat(ttype: IToken*): Boolean = {
    if (ttype.contains(next)) {
      eat()
      true
    }
    else
      false
  }

  protected def eat(ttype: IToken*): IToken = {
    if (ttype.contains(next))
      eat()
    else
      throw new RaptorError(s"Unexpected token $next. Expected $ttype")
  }

  protected def next(tokens: IToken*): Boolean = {
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

  protected def next: IToken = lexer.get(0)

}
