package main.scala.exercise4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case Some(a) => Some(a)
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) => if (f(a)) Some(a) else None
    case None => None
  }


}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]


object Main {
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

  }
}