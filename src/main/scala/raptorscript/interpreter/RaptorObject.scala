package raptorscript.interpreter

import raptorscript.symbol._
import raptorscript.memory.MemorySpace


class RObject(val oType: Type)

class RInstance(override val oType: Type) extends RObject(oType) {

  val memory: MemorySpace = new MemorySpace(oType.name+" instance")
}

object RNullType extends Type {override val name = "NULL" }
object RNull extends RObject(RNullType) {
  override def toString(): String = {
    "NULL"
  }
}
