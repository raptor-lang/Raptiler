package raptorscript

import Console._

object Cli extends App {
  val interpreter = new Interpreter()
  println(s"$BOLD${GREEN}Welcome to$BOLD$MAGENTA RaptorScript$BOLD$GREEN - When you see it, its already too late$RESET")
  while (true) {
    val text = readLine(s"\n$GREEN>>> $RESET")
    if (text != null) {
      try {
        val parser = new Parser(new Lexer(text))
        val result = interpreter.interpret(parser)
        print(result)
      } catch {
        case e: Throwable =>
          print(s"\n$BOLD$RED----- ERROR -----$RESET\n")
          print(e.toString)
          print(s"\n$BOLD$RED-----------------$RESET\n")

      }
    }
  }
}
