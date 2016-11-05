package raptorscript.interpreter

import raptorscript.symbol._
import raptorscript.RaptorError
import raptorscript.IToken
import raptorscript.Tokens._

abstract class RPrimitive[T](override val oType: BuiltInTypeSymbol, val value: T) extends RObject(oType) {

  def binOp(op: IToken, target: RObject): RObject

  override def toString(): String = {
    value.toString()
  }
}

trait RNumeric extends RObject {
  def asFloat(): Float
}

case class RInteger(override val value: Int) extends RPrimitive[Int](SInteger, value) with RNumeric {

  def asFloat(): Float = value.toFloat

  def binOp(op: IToken, target: RObject): RObject = {
    op match {
      case PLUS | MINUS | ASTERISK | SLASH => {
        target match {
          case t: RNumeric => {
            val n: Float = op match {
              case PLUS => this.value + t.asFloat()
              case MINUS => this.value - t.asFloat()
              case ASTERISK => this.value * t.asFloat()
              case SLASH => this.value / t.asFloat()
            }
            return new RFloat(n)
          }
          case _ => throw new RaptorError(s"Can't $op $oType to ${target.oType}")
        }
      }
      case LESS_THAN | GREATER_THAN | EQEQ | NEQ | LTEQ | GTEQ => {
        target match {
          case t: RNumeric => {
            val n: Boolean = op match {
              case LESS_THAN => this.value < t.asFloat()
              case GREATER_THAN => this.value > t.asFloat()
              case NEQ => this.value != t.asFloat()
              case EQEQ => this.value == t.asFloat()
              case LTEQ => this.value <= t.asFloat()
              case GTEQ => this.value >= t.asFloat()
            }
            return new RBool(n)
          }
          case _ => throw new RaptorError(s"Can't compare $oType to ${target.oType}")
        }
      }
    }
  }
}

case class RFloat(override val value: Float) extends RPrimitive[Float](SFloat, value) with RNumeric {

  def asFloat(): Float = value
  def binOp(op: IToken, target: RObject): RObject = {
    op match {
      case PLUS | MINUS | ASTERISK | SLASH => {
        target match {
          case t: RNumeric => {
            val n: Float = op match {
              case PLUS => this.value + t.asFloat()
              case MINUS => this.value - t.asFloat()
              case ASTERISK => this.value * t.asFloat()
              case SLASH => this.value / t.asFloat()
            }
            return new RFloat(n)
          }
          case _ => throw new RaptorError(s"Can't $op $oType and ${target.oType}")
        }
      }
      case LESS_THAN | GREATER_THAN | EQEQ | NEQ | LTEQ | GTEQ => {
        target match {
          case t: RNumeric => {
            val n: Boolean = op match {
              case LESS_THAN => this.value < t.asFloat()
              case GREATER_THAN => this.value > t.asFloat()
              case NEQ => this.value != t.asFloat()
              case EQEQ => this.value == t.asFloat()
              case LTEQ => this.value <= t.asFloat()
              case GTEQ => this.value >= t.asFloat()
            }
            return new RBool(n)
          }
          case _ => throw new RaptorError(s"Can't compare $oType to ${target.oType}")
        }
      }
    }
  }
}

case class RBool(override val value: Boolean) extends RPrimitive[Boolean](SBool, value) {

  def binOp(op: IToken, target: RObject): RObject = {
    op match {
      case EQEQ | NEQ => {
        target match {
          case t: RBool => {
            val n: Boolean = op match {
              case NEQ => value != t.value
              case EQEQ => value == t.value
            }
            RBool(n)
          }
        }
      }
      case _ => throw new RaptorError(s"$oType does not support the operator $op")
    }
  }
}

case class RString(override val value: String) extends RPrimitive[String](SString, value) {

  def binOp(op: IToken, target: RObject): RObject = {
    op match {
      case PLUS | EQEQ | NEQ=> {
        target match {
          case t: RString => {
            op match {
              case NEQ => RBool(value != t.value)
              case EQEQ => RBool(value == t.value)
              case PLUS => new RString(value + t.value)
            }
          }
        }
      }
      case _ => throw new RaptorError(s"$oType does not support the operator $op")
    }
  }

}


