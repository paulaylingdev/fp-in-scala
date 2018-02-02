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
    println(valid.flatMap(a => Left("some error!")))
    println(error.flatMap(a => Left("some error!")))

  }
}