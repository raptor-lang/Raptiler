package raptorscript.memory

import scala.collection.mutable.Map

import raptorscript.symbol.FunSymbol
import raptorscript.interpreter.RObject


class MemorySpace(val name: String) {
  private val members: Map[String, RObject] = Map()

  def get(name: String): Option[RObject] = {
    members.get(name)
  }

  def define(name: String, value: RObject): Unit = {
    members.put(name, value)
  }

  override def toString(): String = {
    val membs = members.map(_.toString()).mkString(", ")
    s"$name: [$membs]"
  }
}

class FunctionSpace(val fun: FunSymbol) extends MemorySpace(fun.name+" invokation")
