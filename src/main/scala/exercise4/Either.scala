package main.scala.exercise4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
    aa <- this
    bb <- b
  } yield f(aa, bb)


}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object EitherMain {
  def main(args: Array[String]): Unit = {
    def printBlueLine(a: Any): Unit =
      println(Console.BLUE + a + Console.RESET)

    val valid = Right(5)
    val error = Left("Error")

    printBlueLine("map")
    println(valid.map(a => a * a))
    println(error.map(a => a))

    printBlueLine("flatMap")
    println(valid.flatMap(a => Right(a * a)))
    println(valid.flatMap(_ => Left("some error!")))
    println(error.flatMap(_ => Left("some error!")))

    printBlueLine("orElse")
    println(valid.orElse(Left("nope")))
    println(valid.orElse(Right(6)))
    println(error.orElse(Left("nope")))
    println(error.orElse(Right(21)))

    printBlueLine("map2")
    println(valid.map2(Right(6))((a, b) => Right(0.1 * a * b)))
    println(valid.map2(Left("map2 Error!"))((a, _) => Right(0.1 * a)))
    println(error.map2(Right(6))((_, b) => Right(0.1 * b)))

  }
}