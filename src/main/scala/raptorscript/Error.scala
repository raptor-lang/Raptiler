package raptorscript

class RaptorError(message: String = "") extends Exception {

  override def toString(): String = message
}
