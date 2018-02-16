package main.scala.exercise5

sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  //  def toList: List[A] = this match {
  //    case Empty => List()
  //    case Cons(h, t) => h() :: t().toList
  //  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n - 1))
    case _ => Empty
  }

  def take2(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A], x: Int): Stream[A] = s match {
      case Cons(h, t) if x > 0 => go(t(), Cons(h, () => acc), x - 1)
      case _ => acc
    }

    go(this, Empty, n) //.reverse
  }
}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def main(args: Array[String]): Unit = {
    def printBlueLine(a: Any): Unit =
      println(Console.BLUE + a + Console.RESET)

    val testStream = Stream(1, 2, 3, 4, 5)

    printBlueLine("headOption")
    println(testStream.headOption)

    printBlueLine("toList")
    println(testStream.toList)
    println(Stream().toList)

    printBlueLine("take")
    println(testStream.take(2).toList)
    println(testStream.take(4).toList)
    println(testStream.take(6).toList)
  }
}
