import scala.collection.mutable.ArrayBuffer
import scala.io.Source._

abstract class Token
case class num(data: Int)        extends Token
case class real(data: Double)    extends Token
case class char(data: Char)      extends Token
case class escword(data: String) extends Token
case class word(data: String)    extends Token
case class fiop(data: Operator)  extends Token

abstract class Operator
case class lambda(data: String)  extends Operator
case class coreOp(data: String)  extends Operator
case class stackOp(data: String) extends Operator
case class basicOp(data: String) extends Operator

var debug = false
var done = false
var evalStack = ArrayBuffer[Token]()
var envStacks = Map[String,ArrayBuffer[Token]]()
var valNames = Set[String]()
var funNames = Set[String]()
var parenCount = 0
var parenPos = 0
var lambdaCount = 0

def parse(in : String) {
  in.replaceAll("//.*$", "").split("\\s+").foreach( i => eval(makeToken(i)));
}

def makeToken(in : String): Token = {
  if (in.charAt(0).isDigit) {
    if (in contains ".")
      return real(in.toDouble)
    else
      return num(in.toInt)
  }
  else if (in.charAt(0) == ''') {
    return char(in.charAt(1))
  }
  else if (in.charAt(0) == '\\')
    return escword(in.slice(1, in.size))
  return in match {
    case "("     => fiop(lambda("lParen"))
    case ")"     => fiop(lambda("rParen"))
    case "DEBUG" => fiop(coreOp("DEBUG"))
    case "DONE"  => fiop(coreOp("DONE"))
    case "LOAD"  => fiop(coreOp("LOAD"))
    case "@"     => fiop(coreOp("apply"))
    case "#"     => fiop(coreOp("copy"))
    case "^"     => fiop(coreOp("cut"))
    case "~"     => fiop(coreOp("insert"))
    case "`"     => fiop(coreOp("show"))
    case "_"     => fiop(coreOp("discard"))
    case ";"     => fiop(coreOp("cap"))
    case ";val"  => fiop(coreOp("valCap"))
    case ";fun"  => fiop(coreOp("funCap"))
    case "clear" => fiop(coreOp("clear"))
    case "empty" => fiop(coreOp("empty"))
    case "="     => fiop(basicOp("equal"))
    case ">"     => fiop(basicOp("greater"))
    case "<"     => fiop(basicOp("less"))
    case "+"     => fiop(basicOp("plus"))
    case "-"     => fiop(basicOp("minus"))
    case "*"     => fiop(basicOp("times"))
    case "/"     => fiop(basicOp("divide"))
    case "%"     => fiop(basicOp("modulo"))
    case _       => word(in)
  }
}

def compute(op : Operator) :Unit = {
  op match {
    case coreOp(x) => x match {
      case "DEBUG" =>
        debug = !debug
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
        eval(fiop(coreOp("cap")))
      case "funCap" =>
        val a = evalStack.last match { case word(x) => x}
        funNames += a
        eval(fiop(coreOp("cap")))
      case "clear" =>
      case "empty" =>
        if (evalStack.isEmpty)
          eval(word("true"))
        else
          eval(word("false"))
      case _       =>
        println("????")
    }

    case stackOp(x) => x match {
      case _       => println("????")
    }

    case basicOp(x) => x match {
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
  }
}

def eval(tok : Token) :Unit = {
  if (debug)
    println(tok);
  tok match {
    case num(x)     => evalStack += num(x)
    case real(x)    => evalStack += real(x)
    case char(x)    => evalStack += char(x)
    case escword(x) => evalStack += word(x)
    case word(x)    => evalStack += word(x)
      if (valNames contains x) eval(fiop(coreOp("apply")))
      else if (funNames contains x) { eval(fiop(coreOp("apply"))); eval(fiop(coreOp("apply"))) }

    case fiop(lambda(x))  =>
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
    case fiop(x) =>
      if (parenCount == 0)
        compute(x)
      else
        evalStack += tok
  }
}

while (!done) {
  print("| ")
  evalStack.foreach( i => print(i + " "))
  print('\n')

  parse(readLine(": "))
}
