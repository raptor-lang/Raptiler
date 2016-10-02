package raptorscript.memory

import scala.collection.mutable.Map

import raptorscript.scope.FunSymbol


class MemorySpace(val name: String) {
  private val members: Map[String, Any] = Map()

  def get(name: String): Option[Any] = {
    members.get(name)
  }

  def define(name: String, value: Any): Unit = {
    members.put(name, value)
  }

  override def toString(): String = {
    val membs = members.map(_.toString()).mkString(", ")
    s"$name: [$membs]"
  }
}

class FunctionSpace(val fun: FunSymbol) extends MemorySpace(fun.name+" invokation")
