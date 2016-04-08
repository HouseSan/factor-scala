//package factorScala.testing

object Implicits {
  implicit def Int2String(i: Int): String = new String("" + i)
  implicit def String2Int(s: String): Int = 1
}

object MainStuff {
  def printNum(s: String) {
    println(s);
  }
}

object Test extends App {
  import Implicits._
  MainStuff.printNum(25)
  var x: Int = 5 + "woof"
  println(x)
  var temp: Int = "woof"
  var y: Int = 5 + temp
  println(y)
}
