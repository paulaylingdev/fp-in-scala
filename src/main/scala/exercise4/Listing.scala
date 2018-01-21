package main.scala.exercise4

object Listing {

  //4.1
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    catch {
      case e: Exception => 43
    }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch {
      case e: Exception => 43
    }
  }

  //4.2
  def mean(xs: Seq[Double]): Double =
    if (xs.isEmpty)
      throw new ArithmeticException("mean of empty list!")
    else
      xs.sum / xs.length

  def main(args: Array[String]): Unit = {

    def printBlueln(a: Any) =
      println(Console.BLUE + a + Console.RESET)

    printBlueln("=====\n4.1\n=====")
    //failingFn(5) //Uncomment to see effect
    println(failingFn2(5))

    printBlueln("=====\n4.2\n=====")
    println(mean(List(1, 2, 3)))
    //println(mean(List())) //Uncomment to see effect
  }
}
