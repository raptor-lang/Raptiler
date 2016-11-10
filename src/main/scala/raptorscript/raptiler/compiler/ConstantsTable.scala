package raptorscript.raptiler.compiler

import scala.collection.mutable.ArrayBuffer
import raptorscript.raptiler.util.ByteUtils


class ConstantsTable {

  private val _bytes = ArrayBuffer[Short]()

  var funCount: Int = 0

  def defineFunc(name: String, argCount: Int, locals: Int, body: Array[Short]): Int = {
    _bytes += 0xF0
    _bytes ++= ByteUtils.byteArray(name)
    _bytes ++= ByteUtils.byteArray(funCount)
    _bytes ++= ByteUtils.byteArray(argCount)
    _bytes ++= ByteUtils.byteArray(locals)
    _bytes ++= ByteUtils.byteArray(body.length + 1) // + 1 for return op
    _bytes ++= body
    _bytes ++= OPs.RETURN()
    funCount += 1
    funCount - 1
  }

  def bytes = _bytes.toArray
}
