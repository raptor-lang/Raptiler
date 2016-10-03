package raptorscript.interpreter

import raptorscript.symbol.{Type, BuiltInTypeSymbol}
import raptorscript.memory.MemorySpace


class RObject(val oType: Type)

class RPrimitive[T](override val oType: BuiltInTypeSymbol, val value: T) extends RObject(oType) {
  override def toString(): String = {
    value.toString()
  }
}

class RInstance(override val oType: Type) extends RObject(oType) {

  val memory: MemorySpace = new MemorySpace(oType.name+" instance")
}

object RNullType extends Type {override val name = "NULL" }
object RNull extends RObject(RNullType)
