import scala.collection.mutable.ArrayBuffer
import scala.io.Source._

abstract class Token
case class num(data: Int)        extends Token
case class real(data: Double)    extends Token
case class char(data: Char)      extends Token
case class word(data: String)    extends Token
case class lambda(data: String)  extends Token
case class coreOp(data: String)  extends Token
case class stackOp(data: String) extends Token
case class basicOp(data: String) extends Token

var done = false
var evalStack = ArrayBuffer[Token]()
var envStacks = Map[String,ArrayBuffer[Token]]()
var valNames = Set[String]()
var funNames = Set[String]()
var parenCount = 0
var parenPos = 0
var lambdaCount = 0

def parse(in : String) {
  in.split("\\s+").foreach( i => eval(makeToken(i)));
}

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
      case "("     => lambda("lParen")
      case ")"     => lambda("rParen")
      case "DONE"  => coreOp("DONE")
      case "LOAD"  => coreOp("LOAD")
      case "@"     => coreOp("apply")
      case "#"     => coreOp("copy")
      case "^"     => coreOp("cut")
      case "<<"    => coreOp("insert")
      case "`"     => coreOp("show")
      case "_"     => coreOp("discard")
      case ";"     => coreOp("cap")
      case ";val"  => coreOp("valCap")
      case ";fun"  => coreOp("funCap")
      case "clear" => coreOp("clear")
      case "empty" => coreOp("empty")
      case "="     => basicOp("equal")
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
      if (valNames contains x) eval(coreOp("apply"))
      else if (funNames contains x) { eval(coreOp("apply")); eval(coreOp("apply")) }

    case lambda(x)  =>
      println("lambda " + x);
      x match {
        case "lParen" =>
          if (parenCount == 0) parenPos = evalStack.size
          parenCount += 1
        case "rParen" =>
          parenCount -= 1
          if (parenCount == 0) {
            val name = lambdaCount.toString
            lambdaCount += 1
            val copy = ArrayBuffer[Token]()
            evalStack.slice(parenPos, evalStack.size).copyToBuffer(copy)
            evalStack.reduceToSize(parenPos)
            envStacks += (name -> copy)
            eval(word(name))
          }
      }

    case coreOp(x)  => println("coreOp " + x);
      if (parenCount == 0) {
        x match {
          case "DONE" =>
            done = true
          case "LOAD" =>
            val n = evalStack.last match { case word(x) => x }
            evalStack.trimEnd(1)
            fromFile(n).getLines.foreach(parse)
          case "apply" =>
            val name : String = evalStack.last match { case word(x) => x }
            evalStack.trimEnd(1)
            envStacks(name).foreach(eval)
          case "copy" =>
            val n = evalStack.last match { case num(x) => x }
            evalStack.trimEnd(1)
            evalStack += evalStack(evalStack.size - n - 1)
          case "cut" =>
            val n = evalStack.last match { case num(x) => x }
            evalStack.trimEnd(1)
            evalStack += evalStack.remove(evalStack.size - n - 1)
          case "insert" =>
            val n = evalStack.last match { case num(x) => x }
            evalStack.trimEnd(1)
            val a = evalStack.last
            evalStack.trimEnd(1)
            evalStack.insert(evalStack.size - n, a)
          case "show" =>
            println(evalStack.last)
          case "discard" =>
            evalStack.trimEnd(1)
          case "cap" =>
            val name = evalStack.last match { case word(x) => x }
            evalStack.trimEnd(1)
            val copy = ArrayBuffer[Token]()
            evalStack.copyToBuffer(copy)
            envStacks += (name -> copy)
            evalStack.clear
          case "valCap" =>
            val a = evalStack.last match { case word(x) => x}
            valNames += a
            eval(coreOp("cap"))
          case "funCap" =>
            val a = evalStack.last match { case word(x) => x}
            funNames += a
            eval(coreOp("cap"))
          case "clear" =>
          case "empty" =>
            if (evalStack.isEmpty)
              eval(word("true"))
            else
              eval(word("false"))
          case _       =>
            println("????")
        }
      } else evalStack += coreOp(x)

    case stackOp(x) => println("stackOp " + x)
      if (parenCount == 0) {
        x match {
          case _       => println("????")
        }
      } else evalStack += stackOp(x)

    case basicOp(x) =>
      println("basicOp " + x)
      if (parenCount == 0) {
        x match {
          case "equal" =>
            val a = evalStack.last
            evalStack.trimEnd(1)
            val b = evalStack.last
            evalStack.trimEnd(1)
            eval(if (a == b) word("true") else word("false"))
          case "greater" =>
            val a = evalStack.last match { case num(x) => x case real(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case num(x) => x case real(x) => x}
            evalStack.trimEnd(1)
            eval(if (a > b) word("true") else word("false"))
          case "less" =>
            val a = evalStack.last match { case num(x) => x case real(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case num(x) => x case real(x) => x}
            evalStack.trimEnd(1)
            eval(if (a < b) word("true") else word("false"))
          case "plus" =>
            val a = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            eval(makeToken((a+b).toString))
          case "minus" =>
            val a = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            eval(makeToken((a-b).toString))
          case "times" =>
            val a = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            eval(makeToken((a*b).toString))
          case "divide" =>
            val a = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            eval(makeToken((a/b).toString))
          case "modulo" =>
            val a = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            val b = evalStack.last match { case real(x) => x case num(x) => x}
            evalStack.trimEnd(1)
            eval(makeToken((a%b).toString))
          case _       => println("????")
        }
      } else evalStack += basicOp(x)
  }
}

while (!done) {
  parse(readLine())
}
