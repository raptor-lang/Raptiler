package raptorscript.compiler

import raptorscript.RaptorError
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

import raptorscript.Parser
import raptorscript.symbol._
import raptorscript.ast._

class Compiler() {
  val scopeStack = new ScopeStack()
  val ast = ListBuffer[Node]()
  val ssb = new SymbolTableBuilder(scopeStack.symtab)
  var bytes = ArrayBuffer[Short]()

  def compile(parser: Parser): Array[Short] = {
    val xs = parser.program
    ssb visit xs
    ast += xs
    visit(xs)
    bytes.toArray
  }

  def visit(node: Node): Unit = {
    node match {
      case n: VarDecl => {
        if (n.value.isDefined) {
          val symb = scopeStack.lookup(n.name).get
          visit(n.value.get)
          bytes ++= OPs.STORE(symb.index)
        }
      }
      case n: VarAssign => {
        val symb = scopeStack.lookup(n.name).get
        visit(n.value)
        bytes ++= OPs.STORE(symb.index)
      }
      case n: VarAccess => {
        val symb = scopeStack.lookup(n.name).get
        bytes ++= OPs.LOAD(symb.index)
      }
      case n: Block => n.list.foreach(visit)
      case n: FunCall => {
        if (n.name == "print") {
          n.args.foreach(visit)
          bytes ++= OPs.PRINT()
          return
        }
        val fs = scopeStack.lookup(n.name).get.asInstanceOf[FunSymbol]
        if (fs.symbols.size != n.args.length)
          throw new raptorscript.RaptorError()
        n.args.foreach(visit)
        // bytes ++= OPs.CALL.asBytes(fs.index)
      }
      case n: LitteralInt =>
        bytes ++= OPs.ICONST(n.value)
      case n: LitteralFloat =>
        // bytes ++= OPs.LOADCONST()
      case n: LitteralBool =>
        bytes ++= OPs.ICONST(if (n.value) 1 else 0)
      case n: LitteralString =>
        // bytes ++= OPs.LOADCONST()
      case n: BinOp => {
        visit(n.left)
        visit(n.right)
        n.token.value match {
          case Some("+") => bytes ++= OPs.ADD()
          case Some("-") => bytes ++= OPs.SUBTRACT()
          case Some("*") => bytes ++= OPs.MULTIPLY()
          case Some("/") => bytes ++= OPs.DIVIDE()
        }
      }
      case n: Program => {
        n.children.foreach(visit)
      }
      case n: IfStatement => {
        visit(n.cond)
        val oldBytes = bytes
        bytes = ArrayBuffer()
        visit(n.block)
        val newBytes = bytes
        val dist = bytes.length + 1 + (if (n.elseBlock.isDefined) OPs.RELJUMP.length else 0)
        bytes = oldBytes
        bytes ++= OPs.RELJUMP_EQ(dist)
        bytes ++= newBytes
        if (n.elseBlock.isDefined) {
          val oldBytes = bytes
          bytes = ArrayBuffer()
          visit(n.elseBlock.get)
          val dist = bytes.length + 1
          val newBytes = bytes
          bytes = oldBytes
          bytes ++= OPs.RELJUMP(dist)
          bytes ++= newBytes
        }
      }
      case n => print("Uncompiled node '" + n + "'")
    }
  }

}

case class Instruction(name: String, opcode: Short, args: InstructionArg*) {
  val length: Int = 1 + args.foldLeft(0)((b,a) => b + a.length)

  def apply(argvs: BigInt*): Array[Short] = {
    val xs = ArrayBuffer[Short](opcode)
    for (i <- args.indices) {
      val a = args(i)
      var av = argvs(i).toByteArray
      if (!a.signed) {
        av = av.tail
      }
      if (a.length < av.length) {
        throw new RaptorError("Argument '" + a.name + "' of instruction " + name + " only takes " + a.length + "bytes. " + av.length + " given!")
      }
      for (j <- 0 until a.length - av.length) {
        xs += 0x00
      }
      xs ++= av.map(_.shortValue())
    }
    xs.toArray
  }
}

case class InstructionArg(name: String, length: Byte, signed: Boolean = true)

object OPs {
  private val INT_LENGTH: Byte = 4
  private val RELJUMP_LENGTH: Byte = 4
  private val MEM_INDEX_LENGTH: Byte = 4

  val NOP = Instruction("NOP", 0x00)
  val HALT = Instruction("HALT", 0x01)
  val ICONST = Instruction("ICONST", 0x02, InstructionArg("value", INT_LENGTH))
  val POP = Instruction("POP", 0x03)
  val ADD = Instruction("ADD", 0x1A)
  val SUBTRACT = Instruction("SUBTRACT", 0x1B)
  val MULTIPLY = Instruction("MULTIPLY", 0x1C)
  val DIVIDE = Instruction("DIVIDE", 0x1D)
  val MODULUS = Instruction("MODULUS", 0x1E)
  val AND = Instruction("AND", 0x11)
  val OR = Instruction("OR", 0x12)
  val NOT = Instruction("NOT", 0x13)
  val RSHIFT = Instruction("RSHIFT", 0x14, InstructionArg("bits", INT_LENGTH, false))
  val LSHIFT = Instruction("LSHIFT", 0x15, InstructionArg("bits", INT_LENGTH, false))
  val COMP = Instruction("COMP", 0x20)
  val COMP_LT = Instruction("COMP_LT", 0x21)
  val COMP_EQ = Instruction("COMP_EQ", 0x22)
  val COMP_GT = Instruction("COMP_GT", 0x23)
  val RELJUMP = Instruction("RELJUMP", 0x2A, InstructionArg("distance", RELJUMP_LENGTH))
  val RELJUMP_GT = Instruction("RELJUMP_GT", 0x2B, InstructionArg("distance", RELJUMP_LENGTH))
  val RELJUMP_LT = Instruction("RELJUMP_LT", 0x2C, InstructionArg("distance", RELJUMP_LENGTH))
  val RELJUMP_EQ = Instruction("RELJUMP_EQ", 0x2D, InstructionArg("distance", RELJUMP_LENGTH))
  val STORE = Instruction("STORE", 0x80, InstructionArg("index", MEM_INDEX_LENGTH))
  val LOAD = Instruction("LOAD", 0x81, InstructionArg("index", MEM_INDEX_LENGTH))
  val PRINT = Instruction("PRINT", 0xA0)
  val DUMP_STACK = Instruction("DUMP_STACK", 0xF0)
}
