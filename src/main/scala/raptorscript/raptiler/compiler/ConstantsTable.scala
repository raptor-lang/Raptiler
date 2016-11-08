package raptorscript.raptiler.compiler

import scala.collection.mutable.ArrayBuffer
import raptorscript.raptiler.util.ByteUtils


class ConstantsTable {

  private val _bytes = ArrayBuffer[Short]()

  var funCount: Int = 0

  def defineFunc(argCount: Int, locals: Int, body: Array[Short]): Int = {
    _bytes += 0xF0
    _bytes ++= ByteUtils.byteArray(funCount, 4)
    _bytes ++= ByteUtils.byteArray(argCount, 4)
    _bytes ++= ByteUtils.byteArray(locals, 4)
    _bytes ++= ByteUtils.byteArray(body.length + 1, 4) // + 1 for return op
    _bytes ++= body
    _bytes ++= OPs.RETURN()
    funCount += 1
    funCount - 1
  }

  def bytes = _bytes.toArray
}
