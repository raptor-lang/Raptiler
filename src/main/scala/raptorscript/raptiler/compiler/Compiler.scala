package raptorscript.raptiler.compiler

import raptorscript.raptiler.RaptorError
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.ArrayBuffer

import raptorscript.raptiler.Parser
import raptorscript.raptiler.symbol._
import raptorscript.raptiler.ast._
import raptorscript.raptiler.util.ByteUtils

class Compiler() {
  val scopeStack = new ScopeStack()
  val ast = ListBuffer[Node]()
  val ssb = new SymbolTableBuilder(scopeStack.symtab)
  var bytes = ArrayBuffer[Short]()
  val constTab = new ConstantsTable()

  def compile(parser: Parser): Array[Short] = {
    val xs = parser.program
    ssb visit xs
    ast += xs
    visit(xs)
    bytes.toArray
  }

  def op(bytes: Array[Short]): Unit = {
    this.bytes ++= bytes
  }

  def visit(node: Node): Unit = {
    node match {
      case n: VarDecl => {
        if (n.value.isDefined) {
          val symb = scopeStack.lookup(n.name).get
          visit(n.value.get)
          op(OPs.STORE(symb.index))
        }
      }
      case n: VarAssign => {
        val symb = scopeStack.lookup(n.name).get
        visit(n.value)
        op(OPs.STORE(symb.index))
      }
      case n: VarAccess => {
        val symb = scopeStack.lookup(n.name).get
        op(OPs.LOAD(symb.index))
      }
      case n: Block => n.list.foreach(visit)
      case n: FunDecl => {
        val fs = scopeStack.lookup(n.name).get.asInstanceOf[FunSymbol]
        val newBytes = ArrayBuffer[Short]()
        val oldBytes = bytes
        bytes = newBytes
        scopeStack.push(fs)
        scopeStack.push(fs.bodyScope)
        visit(fs.body)
        scopeStack.pop()
        scopeStack.pop()
        bytes = oldBytes
        fs.index = constTab.defineFunc(fs.symbCount, fs.bodyScope.symbCount, newBytes.toArray)
      }
      case n: FunCall => {
        if (n.name == "print") {
          n.args.foreach(visit)
          op(OPs.PRINT())
          return
        }
        val fs = scopeStack.lookup(n.name).get.asInstanceOf[FunSymbol]
        n.args.reverse.foreach(visit)
        op(OPs.CALL(fs.index))
      }
      case n: LitteralInt =>
        op(OPs.ICONST(n.value))
      case n: LitteralFloat =>
        // op(OPs.LOADCONST())
      case n: LitteralBool =>
        op(OPs.ICONST(if (n.value) 1 else 0))
      case n: LitteralString =>
        // op(OPs.LOADCONST())
      case n: BinOp => {
        visit(n.right)
        visit(n.left)
        n.token.value match {
          case Some("+") => op(OPs.ADD())
          case Some("-") => op(OPs.SUBTRACT())
          case Some("*") => op(OPs.MULTIPLY())
          case Some("/") => op(OPs.DIVIDE())
          case Some(">") => op(OPs.COMP_GT())
          case Some("<") => op(OPs.COMP_LT())
          case Some("==") => op(OPs.COMP_EQ())
          case Some("!=") => {
            op(OPs.COMP_EQ())
            op(OPs.NOT())
          }
          case Some(">=") => {
            op(OPs.COMP_LT())
            op(OPs.NOT() )
          }
          case Some("<=") => {
            op(OPs.COMP_GT())
            op(OPs.NOT())
          }
          case v => throw new RaptorError(s"BinOp '$v' not implemented")
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
        op(OPs.RELJUMP_EQ(dist))
        bytes ++= newBytes
        if (n.elseBlock.isDefined) {
          val oldBytes = bytes
          bytes = ArrayBuffer()
          visit(n.elseBlock.get)
          val dist = bytes.length + 1
          val newBytes = bytes
          bytes = oldBytes
          op(OPs.RELJUMP(dist))
          bytes ++= newBytes
        }
      }
      case n => print(s"Uncompiled node '$n'")
    }
  }

  def globCount: Int = GlobalScope.symbCount

}

case class Instruction(name: String, opcode: Short, args: OpArg*) {
  val length: Int = 1 + args.foldLeft(0)((b,a) => b + a.length)

  def apply(argvs: Int*): Array[Short] = {
    val xs = ArrayBuffer[Short](opcode)
    for (i <- args.indices) {
      val a = args(i)
      var av = ByteUtils.byteArray(argvs(i))
      if (!a.signed) {
        av = av.tail
      }
      if (a.length < av.length) {
        throw new RaptorError(s"Argument '${a.name}' of instruction $name only takes ${a.length} bytes. ${av.length} bytes given!")
      }
      xs ++= av
    }
    xs.toArray
  }
}

case class OpArg(name: String, length: Byte, signed: Boolean = true)

object OPs {
  private val INT_LENGTH: Byte = 4
  private val RELJUMP_LENGTH: Byte = 4
  private val MEM_INDEX_LENGTH: Byte = 4

  val NOP = Instruction("NOP", 0x00)
  val HALT = Instruction("HALT", 0x01)
  val ICONST = Instruction("ICONST", 0x02, OpArg("value", INT_LENGTH))
  val POP = Instruction("POP", 0x03)
  val ADD = Instruction("ADD", 0x1A)
  val SUBTRACT = Instruction("SUBTRACT", 0x1B)
  val MULTIPLY = Instruction("MULTIPLY", 0x1C)
  val DIVIDE = Instruction("DIVIDE", 0x1D)
  val MODULUS = Instruction("MODULUS", 0x1E)
  val AND = Instruction("AND", 0x11)
  val OR = Instruction("OR", 0x12)
  val NOT = Instruction("NOT", 0x13)
  val RSHIFT = Instruction("RSHIFT", 0x14, OpArg("bits", INT_LENGTH, false))
  val LSHIFT = Instruction("LSHIFT", 0x15, OpArg("bits", INT_LENGTH, false))
  val COMP = Instruction("COMP", 0x20)
  val COMP_LT = Instruction("COMP_LT", 0x21)
  val COMP_EQ = Instruction("COMP_EQ", 0x22)
  val COMP_GT = Instruction("COMP_GT", 0x23)
  val RELJUMP = Instruction("RELJUMP", 0x2A, OpArg("distance", RELJUMP_LENGTH))
  val RELJUMP_GT = Instruction("RELJUMP_GT", 0x2B, OpArg("distance", RELJUMP_LENGTH))
  val RELJUMP_LT = Instruction("RELJUMP_LT", 0x2C, OpArg("distance", RELJUMP_LENGTH))
  val RELJUMP_EQ = Instruction("RELJUMP_EQ", 0x2D, OpArg("distance", RELJUMP_LENGTH))
  val STORE = Instruction("STORE", 0x80, OpArg("index", MEM_INDEX_LENGTH))
  val LOAD = Instruction("LOAD", 0x81, OpArg("index", MEM_INDEX_LENGTH))
  val CALL = Instruction("CALL", 0x90, OpArg("id", 4))
  val RETURN = Instruction("RETURN", 0x91)
  val PRINT = Instruction("PRINT", 0xA0)
  val DUMP_STACK = Instruction("DUMP_STACK", 0xF0)
}
