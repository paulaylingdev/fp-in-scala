package main.scala.exercise5

import Stream._

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
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def take2(n: Int): Stream[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: Stream[A], x: Int): Stream[A] = s match {
      case Cons(h, t) if x > 0 => go(t(), cons(h(), acc), x - 1)
      case _ => acc
    }

    go(this, empty, n) //.reverse
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )

  def headOption2: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (f(h)) cons(h, t)
      else t
    )

  def append[B >: A](b: => Stream[B]): Stream[B] =
    foldRight(b)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

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

  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = cons(a, tail)
    tail
  }

  def main(args: Array[String]): Unit = {
    def printBlueLine(a: Any): Unit =
      println(Console.BLUE + a + Console.RESET)

    val testStream = Stream(1, 2, 3, 4, 5)

    printBlueLine("headOption")
    println(testStream.headOption)
    println(Stream().headOption)

    printBlueLine("toList")
    println(testStream.toList)
    println(Stream().toList)

    printBlueLine("take")
    println(testStream.take(2).toList)
    println(testStream.take(4).toList)
    println(testStream.take(6).toList)
    println(Stream().take(1).toList)

    printBlueLine("drop")
    println(testStream.drop(1).toList)
    println(testStream.drop(3).toList)
    println(testStream.drop(6).toList)

    printBlueLine("takeWhile")
    println(testStream.takeWhile(x => x < 3).toList)
    println(testStream.takeWhile(x => x < 5).toList)

    printBlueLine("exists")
    println(testStream.exists(x => x == 4))
    println(testStream.exists(x => x == 6))

    printBlueLine("forAll")
    println(testStream.forAll(x => x > 1))
    println(testStream.forAll(x => x < 6))

    printBlueLine("takeWhile2")
    println(testStream.takeWhile2(x => x < 3).toList)
    println(testStream.takeWhile2(x => x < 5).toList)
    println(testStream.takeWhile2(_ => false).toList)

    printBlueLine("headOption2")
    println(testStream.headOption2)
    println(Stream().headOption2)

    printBlueLine("map")
    println(testStream.map(i => i * 0.5).toList)
    println(Stream().map(x => x))

    printBlueLine("filter")
    println(testStream.filter(i => (i % 2) == 0).toList)
    println(testStream.filter(i => i > 5))

    printBlueLine("append")
    println(testStream.append(Stream(6)).toList)
    println(Stream().append(Stream(1)).toList)

    printBlueLine("flatMap")
    println(testStream.flatMap(i => if(i == 2) Stream(i) else empty).toList)
    println(testStream.flatMap(_ => empty).toList)

    printBlueLine("find")
    println(testStream.find(_ == 3))

    printBlueLine("constant")
    println(constant("blah").take(3).toList)
  }
}
