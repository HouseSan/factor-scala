import scala.collection.mutable.{ArrayBuffer, LinkedHashMap}
import scala.io.Source._
import scala.util.matching.Regex
import java.lang.IllegalArgumentException

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

val allOps = Map[String, Operator](
  "("     -> lambda("lParen"),
  ")"     -> lambda("rParen"),
  "DEBUG" -> coreOp("DEBUG"),
  "DONE"  -> coreOp("DONE"),
  "LOAD"  -> coreOp("LOAD"),
  "@"     -> coreOp("apply"),
  "#"     -> coreOp("copy"),
  "^"     -> coreOp("cut"),
  "~"     -> coreOp("insert"),
  "`"     -> coreOp("show"),
  "_"     -> coreOp("discard"),
  ";"     -> coreOp("cap"),
  ";@"  -> coreOp("valCap"),
  ";@@"  -> coreOp("funCap"),
  "clear" -> coreOp("clear"),
  "empty" -> coreOp("empty"),
  "$#"    -> stackOp("copy"),
  "$^"    -> stackOp("cut"),
  "$~"    -> stackOp("insert"),
  "$`"    -> stackOp("show"),
  "$_"    -> stackOp("discard"),
  "$="    -> stackOp("equal"),
  "="     -> basicOp("equal"),
  ">"     -> basicOp("greater"),
  "<"     -> basicOp("less"),
  "+"     -> basicOp("plus"),
  "-"     -> basicOp("minus"),
  "*"     -> basicOp("times"),
  "/"     -> basicOp("divide"),
  "%"     -> basicOp("modulo")
)
val revAllOps = allOps.map(_.swap)

case class Lexic(r: Regex, f: String => Option[Token])

//Mutable, Orderd Map. Traversal in same order as insertion.
val basicOpsMap = LinkedHashMap[String, Operator](
  """("""     -> lambda("lParen"),
  """)"""     -> lambda("rParen"),
  """@"""     -> coreOp("apply"),
  """#"""     -> coreOp("copy"),
  """^"""     -> coreOp("cut"),
  """~"""     -> coreOp("insert"),
  """`"""     -> coreOp("show"),
  """_"""     -> coreOp("discard"),
  """;@@"""   -> coreOp("funCap"),
  """;@"""    -> coreOp("valCap"),
  """;"""     -> coreOp("cap"),
  """$#"""    -> stackOp("copy"),
  """$^"""    -> stackOp("cut"),
  """$~"""    -> stackOp("insert"),
  """$`"""    -> stackOp("show"),
  """$_"""    -> stackOp("discard"),
  """$="""    -> stackOp("equal"),
  """="""     -> basicOp("equal"),
  """>"""     -> basicOp("greater"),
  """<"""     -> basicOp("less"),
  """+"""     -> basicOp("plus"),
  """-"""     -> basicOp("minus"),
  """*"""     -> basicOp("times"),
  """/"""     -> basicOp("divide"),
  """%"""     -> basicOp("modulo")
)
val wordOpsMap = LinkedHashMap[String, Token](
  "DEBUG" -> fiop(coreOp("DEBUG")),
  "DONE"  -> fiop(coreOp("DONE")),
  "LOAD"  -> fiop(coreOp("LOAD")),
  "clear" -> fiop(coreOp("clear")),
  "empty" -> fiop(coreOp("empty"))
)


val wSpaceR = Lexic("""(\s+)(.*)""".r, {_ => None})
val realR = Lexic("""(-?\d+\.\d+)(.*)""".r, {x => Some(real(x.toDouble))})
val intR = Lexic("""(-?\d+)(.*)""".r, {x => Some(num(x.toInt))})
val wordR = Lexic("""(\w+)(.*)""".r, {x => Some(wordOpsMap.getOrElse(x, word(x)))})


//  val lParenR = Lexic(
//val tempLexicList = List(wSpaceR, realR, intR, wordR)
val lexicList = List(wSpaceR, realR, intR, wordR) ++ basicOpsMap.map({case (s, op) => Lexic(s"(${Regex.quote(s)})(.*)".r, {_ => Some(fiop(op))} )})

var debug = false
var done = false
var evalStack = ArrayBuffer[Token]()
var envStacks = Map[String,ArrayBuffer[Token]]()
var valNames = Set[String]()
var funNames = Set[String]()
var parenCount = 0
var parenPos = 0
var lambdaCount = 0

class FiStack (ar : ArrayBuffer[Token]) {
  def pop() : Token = {
    val ret = ar.last
    ar.trimEnd(1)
    return ret
  }
  def pos(n:Int) : Int = {
    return ar.size - n - 1
  }
}
implicit def testing(s: ArrayBuffer[Token]) = new FiStack(s)

def toString(tok : Token) : String = {
  return tok match {
    case num(x) => x.toString
    case real(x) => x.toString
    case char(x) => x.toString
    case escword(x) => "\\" + x.toString
    case word(x) => "\\" + x.toString
    case fiop(x) => revAllOps(x)
  }
}

def parse(in : String) : List[Token] = {
  var tokenList : List[Token] = List[Token]()
  var curString : String = in
  while (!curString.isEmpty) {
    makeToken(curString) match {
      case (Some(t), s) => curString = s;
        tokenList = tokenList :+ t // O(n) lol
      case (None, s) => curString = s
    }
  }
  return tokenList
}

def makeToken(in : String): (Option[Token], String) = {

  for(lexic <- lexicList) {
    lexic.r.unapplySeq(in) match {
      case Some(List(m, r)) => return (lexic.f(m), r)
      case None =>
    }
  }
  throw new IllegalArgumentException(in)
  return (None, "")

/*  if (in.charAt(0).isDigit || (in.charAt(0) == '-' && in.length > 1)) {
    return {
      if (in contains ".") real(in.toDouble)
      else num(in.toInt)
    }
  }
  else if (in.charAt(0) == ''') {
    if (in.charAt(1) != '\\')
      return char(in.charAt(1))
    else
      return char (in.charAt(2) match {
        case 't' => '\t'
        case 'n' => '\n'
        case '0' => '\0'
        case 'x' => Integer.parseInt(in.slice(3, in.size), 16).toChar
        case _   => ' '
      })
  }
  else if (in.charAt(0) == '\\')
    return escword(in.slice(1, in.size))
  else if (allOps contains in)
    return fiop(allOps(in))
  return word(in)
*/
}

def compute(op : Operator) :Unit = {
  op match {
    case coreOp(x) => x match {
      case "DEBUG"   => debug = !debug
      case "DONE"    => done = true
      case "LOAD"    =>
        val n = evalStack.pop match { case word(x) => x }
        fromFile(n).getLines.foreach(parse)
      case "apply"   =>
        val name: String = evalStack.pop match { case word(x) => x }
        envStacks(name).foreach(eval)
      case "copy"    =>
        val n = evalStack.pop match { case num(x) => x }
        evalStack += evalStack(evalStack.pos(n))
      case "cut"     =>
        val n = evalStack.pop match { case num(x) => x }
        evalStack += evalStack.remove(evalStack.pos(n))
      case "insert"  =>
        val n = evalStack.pop match { case num(x) => x }
        val a = evalStack.pop
        evalStack.insert(evalStack.size - n, a)
      case "show"    => print(toString(evalStack.last))
      case "discard" => evalStack.pop
      case "cap"     =>
        val name = evalStack.pop match { case word(x) => x }
        val copy = ArrayBuffer[Token]()
        evalStack.copyToBuffer(copy)
        envStacks += (name -> copy)
        evalStack.clear
      case "valCap"  => valNames += (evalStack.last match { case word(x) => x})
        eval(fiop(coreOp("cap")))
      case "funCap"  => funNames += (evalStack.last match { case word(x) => x})
        eval(fiop(coreOp("cap")))
      case "clear"   => evalStack.clear
      case "empty"   => eval(word(if (evalStack.isEmpty) "true" else "false"))
      case _         => println("????")
    }

    case stackOp(x) => x match {
      case "copy"    =>
        val n = evalStack.pop match { case num(x) => x }
        val name = evalStack.last match { case word(x) => x}
        evalStack += envStacks(name)(envStacks(name).pos(n))
      case "cut"     =>
        val n = evalStack.pop match { case num(x) => x }
        val name = evalStack.last match { case word(x) => x}
        evalStack += envStacks(name).remove(envStacks(name).pos(n))
      case "insert"  =>
        val n = evalStack.pop match { case num(x) => x }
        val a = evalStack.pop
        val name = evalStack.last match { case word(x) => x}
        envStacks(name).insert(envStacks(name).pos(n)+1, a)
      case "show"    =>
        val name = evalStack.last match { case word(x) => x}
        envStacks(name).foreach( i => print(toString(i) + " ") )
        print('\n')
      case "discard" =>
        val name = evalStack.pop match { case word(x) => x}
        if (valNames contains name)
          valNames -= name
        if (funNames contains name)
          funNames -= name
      case "equal"   =>
      case _         => println("????")
    }

    case basicOp(x) => x match {
      case "equal" =>
        val a = evalStack.pop
        val b = evalStack.pop
        eval(if (a == b) word("true") else word("false"))
      case "greater" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(if (a > b) word("true") else word("false"))
      case "less" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(if (a < b) word("true") else word("false"))
      case "plus" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(real(a+b))
      case "minus" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(real(a-b))
      case "times" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(real(a*b))
      case "divide" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(real(a/b))
      case "modulo" =>
        val a = evalStack.pop match { case real(x) => x case num(x) => x}
        val b = evalStack.pop match { case real(x) => x case num(x) => x}
        eval(real(a%b))
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
      if (parenCount == 0) {
        if (valNames contains x) eval(fiop(coreOp("apply")))
        else if (funNames contains x) { eval(fiop(coreOp("apply"))); eval(fiop(coreOp("apply"))) }
      }

    case fiop(lambda(x))  =>
      x match {
        case "lParen" =>
          if (parenCount == 0) parenPos = evalStack.size
          else evalStack += tok
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
          else evalStack += tok
      }
    case fiop(x) =>
      if (parenCount == 0)
        compute(x)
      else
        evalStack += tok
  }
}

def main = {
  while (!done) {
    print("| ")
    evalStack.foreach( i => print(toString(i) + " "))
    print('\n')

    parse(readLine(": ")).foreach(eval(_))
  }
}

main
