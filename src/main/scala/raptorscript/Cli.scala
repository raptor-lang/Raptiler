package raptorscript

import Console._

object Cli extends App {
  val interpreter = new Interpreter()
  println(s"${GREEN}Welcome to$MAGENTA RaptorScript$GREEN - When you see it, its already too late")
  while (true) {
    val text = readLine(s"$GREEN>>> ")
    try {
      val result = interpreter.interpret(text)
      print(result)
    } catch {
      case e: RaptorError => {
        print(e.toString())
      }
    }
  }
}
