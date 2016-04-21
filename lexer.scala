import scala.collection.mutable.ArrayBuffer

abstract class Token
case class num(data: Int)        extends Token
case class real(data: Double)    extends Token
case class char(data: Char)      extends Token
case class word(data: String)    extends Token
case class coreOp(data: String)  extends Token
case class stackOp(data: String) extends Token
case class basicOp(data: String) extends Token

var going = true
var evalStack = ArrayBuffer[Token]()

def makeToken(in : String): Token = {
  if (in.charAt(0).isDigit) {
    if (in contains ".")
      return real(in.toDouble)
    else
      return num(in.toInt)
  } else if (in.charAt(0) == ''') {
    return char(in.charAt(1))
  } else {
    return in match {
      case "@"     => coreOp("apply")
      case "#"     => coreOp("copy")
      case "^"     => coreOp("cut")
      case "`"     => coreOp("show")
      case "_"     => coreOp("discard")
      case "("     => coreOp("lParen")
      case ")"     => coreOp("rParen")
      case ";"     => coreOp("cap")
      case ";val"  => coreOp("valCap")
      case ";fun"  => coreOp("funCap")
      case "clear" => coreOp("clear")
      case "empty" => coreOp("empty")
      case "="     => basicOp("equal")
      case "!"     => basicOp("not")
      case ">"     => basicOp("greater")
      case "<"     => basicOp("less")
      case "+"     => basicOp("plus")
      case "-"     => basicOp("minus")
      case "*"     => basicOp("times")
      case "/"     => basicOp("divide")
      case "%"     => basicOp("modulo")
      case _       => word(in)
    }
  }
}

def eval(tok : Token) :Unit = {
  tok match {
    case num(x)     => println("number " + x.toString); evalStack += num(x)
    case real(x)    => println("real " + x.toString); evalStack += real(x)
    case char(x)    => println("char " + x.toString); evalStack += char(x)
    case word(x)    => println("word " + x); evalStack += word(x)
    case coreOp(x)  => println("coreOp " + x); {
      x match {
        //case "apply" =>
        case "copy" =>
          val n = evalStack.last match { case num(x) => x }
          evalStack.trimEnd(1)
          evalStack += evalStack(evalStack.size - n - 1)
        case "cut" =>
          val n = evalStack.last match { case num(x) => x }
          evalStack.trimEnd(1)
          evalStack += evalStack.remove(evalStack.size - n - 1)
        case "show" => println(evalStack.last)
        case "discard" => evalStack.trimEnd(1)
        //case "lParen" =>
        //case "rParen" =>
        //case "cap" =>
        //case "valCap" =>
        //case "funCap" =>
        case "clear" =>
          evalStack.clear
        case "empty" =>
          if (evalStack.isEmpty)
            eval(word("true"))
          else
            eval(word("false"))
        case _       => println("????")
      }
    }
    case stackOp(x) => println("stackOp " + x); {

    }
    case basicOp(x) => println("basicOp " + x); {

    }
  }
}

while (going) {
  val input = readLine().split(" +");
  input.foreach{ i => eval(makeToken(i)) }
  going = false
}
