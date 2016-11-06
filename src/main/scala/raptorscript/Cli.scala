package raptorscript

import Console._
import java.io.{ BufferedOutputStream, FileOutputStream }

import raptorscript.compiler.Compiler

object Cli extends App {

  val compiler = new Compiler()
  println(s"$BOLD${GREEN}Welcome to$BOLD$MAGENTA RaptorScript$BOLD$GREEN - When you see it, its already too late$RESET")

  if (args.length > 0) {
    if (args(0) == "-i") {
      if (args.length > 1)
        runFile(args(1))
      interactive()
    } else {
      writeFile(args(0) + ".crapt", runFile(args(0)))
    }
  } 

  def runFile(path: String): Array[Short] = {
    val text = io.Source.fromFile(path).mkString
    run(text)
  }

  def writeFile(path: String, bytes: Array[Short]): Unit = {
    // TODO: this header is a hackedyhack
    val header = Array[Short](0x5A, 0xB7, 0x05,0x00,0x00,0x00,0x00,0x08)
    val bos = new BufferedOutputStream(new FileOutputStream(path))
    Stream.continually(bos.write(header.++(bytes).map(_.toByte)))
    bos.close()
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
        print(run(text))
    }
  }

  def run(text: String): Array[Short] = {
    try {
      val parser = new Parser(new Lexer(text))
      val result = compiler.compile(parser)
      result
    } catch {
      case e: Throwable =>
        print(s"\n$BOLD$RED----- ERROR -----$RESET\n")
        print(e.toString)
        print(s"\n$BOLD$RED-----------------$RESET\n")
        Array[Short]()
    }
  }
}
