package raptorscript.raptiler.util

import scala.collection.mutable.ArrayBuffer


object ByteUtils {

  def byteArray(num: Int, length: Int = 4): Array[Short] = {
    val xs = ArrayBuffer[Short]()
    for (i <- 0 to (length - 1)) {
      xs.append((num >>> (8*i)).shortValue())
    }
    xs.reverse.toArray
  }
}
