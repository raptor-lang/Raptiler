package raptorscript.raptiler.util

object ByteUtils {

  def byteArray(num: BigInt, length: Int): Array[Short] = num.toByteArray.takeRight(length).map(_.shortValue())
}
