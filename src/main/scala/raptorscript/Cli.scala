package raptorscript

import Console._

import raptorscript.interpreter.Interpreter

object Cli extends App {

  val interpreter = new Interpreter()
  println(s"$BOLD${GREEN}Welcome to$BOLD$MAGENTA RaptorScript$BOLD$GREEN - When you see it, its already too late$RESET")

  if (args.length > 0) {
    runFile(args(0))
  }

  interactive()

  def runFile(path: String): Unit = {
    val text = io.Source.fromFile(path).mkString
    run(text)
  }

  def interactive(): Unit = {
    while (true) {
      val text = readLine(s"\n$GREEN>>> $RESET")
      if (text.startsWith("/")) {
        text match {
          case "/file" => runFile(args(0))
          case "/exit" => {
            print(s"$GREEN$BOLD Damn! he got away... I guess there is no dinner for the raptors tonight :(\n")
            return
          }
        }
      } else
        run(text)
    }
  }

  def run(text: String): Unit = {
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
