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

  def mean_1(xs: IndexedSeq[Double], onEmpty: Double): Double =
    if (xs.isEmpty) onEmpty
    else xs.sum / xs.length

  def mean_2(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def main(args: Array[String]): Unit = {

    def printBlueln(a: Any) =
      println(Console.BLUE + a + Console.RESET)

    printBlueln("=====\n4.1\n=====")
    //failingFn(5) //Uncomment to see effect
    println(failingFn2(5))

    printBlueln("=====\n4.2\n=====")
    println(mean(List(1, 2, 3)))
    //println(mean(List())) //Uncomment to see effect
    println(mean_1(IndexedSeq(1,2,3), 0))
    println(mean_1(IndexedSeq(), 0))
  }
}
