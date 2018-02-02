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

  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = age * numberOfSpeedingTickets

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
    val optAge: Option[Int] = Try {
      age.toInt
    }
    val optTickets: Option[Int] = Try {
      numberOfSpeedingTickets.toInt
    }
    map2(optAge, optTickets)(insuranceRateQuote)
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(aa => aa)

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

  def traverse_1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((h, t) => map2(f(h), t)(_ :: _))

  def main(args: Array[String]): Unit = {
    def printBlueLine(a: Any): Unit =
      println(Console.BLUE + a + Console.RESET)

    val number5 = Some(5)
    val nothing = None

    printBlueLine("Map")
    println(nothing.map(_ => "bob"))
    println(number5.map(_.toRadians))

    printBlueLine("flatMap")
    println(nothing.flatMap(_ => Some("bob")))
    println(number5.flatMap(a => Some(a.toRadians)))

    printBlueLine("getOrElse")
    println(nothing.getOrElse("bob"))
    println(number5.getOrElse(6))

    printBlueLine("orElse")
    println(nothing.orElse(Some("bob")))
    println(number5.orElse(Some(6)))

    printBlueLine("filter")
    println(number5.filter(_ == 4))
    println(number5.filter(_ == 5))

    printBlueLine("variance")
    println(variance(Seq(1.0, 2.0, 3.0)))
    println(variance(Seq()))

    printBlueLine("map2")
    println(map2(Some(5), Some(2.0))((a, b) => (a * b).toString))

    printBlueLine("insurance")
    println(parseInsuranceRateQuote("12", "4"))
    println(parseInsuranceRateQuote("bob", "4"))

    printBlueLine("sequence / traverse")
    println(sequence(List(Some(1), Some(2), Some(3))))
    println(sequence(List(Some(1), Some(2), None)))

    printBlueLine("traverse")
    println(traverse(List(1, 2, 3))(x => Some(x * x)))
  }
}