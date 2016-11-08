package raptorscript.raptiler

class RaptorError(message: String = "") extends Exception {

  override def toString(): String = message
}
