package main.scala.exercise4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    flatMap(a => if (f(a)) Some(a) else None)

}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Main {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  def insuaranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try(age.toInt)
    val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
//    insuaranceRateQuote(optAge, optTickets)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def main(args: Array[String]): Unit = {
    def printBlueln(a: Any) =
      println(Console.BLUE + a + Console.RESET)

    val number5 = Some(5)
    val nothing = None

    printBlueln("Map")
    println(nothing.map(_ => "bob"))
    println(number5.map(_.toRadians))

    printBlueln("flatMap")
    println(nothing.flatMap(_ => Some("bob")))
    println(number5.flatMap(a => Some(a.toRadians)))

    printBlueln("getOrElse")
    println(nothing.getOrElse("bob"))
    println(number5.getOrElse(6))

    printBlueln("orElse")
    println(nothing.orElse(Some("bob")))
    println(number5.orElse(Some(6)))

    printBlueln("filter")
    println(number5.filter(_ == 4))
    println(number5.filter(_ == 5))

    printBlueln("variance")
    println(variance(Seq(1.0, 2.0, 3.0)))
    println(variance(Seq()))

  }
}