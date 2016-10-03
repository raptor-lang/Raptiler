package raptorscript.memory

import scala.collection.mutable.Stack

import raptorscript.interpreter.RObject


class MemoryStack {

  val globalSpace = new MemorySpace("GLOBALS")
  val stack = Stack[MemorySpace](globalSpace)

  def push(space: MemorySpace = new MemorySpace("")) = stack.push(space)

  def pop() = stack.pop()

  def define(name: String, obj: RObject): Unit = stack.top.define(name, obj)

  def lookup(name: String): Option[RObject] = {
    for (ms <- stack) {
      val o = ms.get(name)
      if (o.isDefined)
        return o
    }
    None
  }

  def update(name: String, obj: RObject): Unit = {
    for (ms <- stack) {
      if (ms.get(name).isDefined)
        return ms.define(name, obj)
    }
    throw new raptorscript.RaptorError("Could not update object reference. No original reerence found")
  }
}
