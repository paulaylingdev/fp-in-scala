package exercise5

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

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def take3(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), x) if x > 0 => Some((h(), (t(), x - 1)))
    case _ => None
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case (Cons(h, t)) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](xs: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, xs)) {
    case (Cons(h, t), Cons(y, ys)) => Some((f(h(), y()), (t(), ys())))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h, t), Cons(x, xs)) => Some(((Some(h()), Some(x())), (t(), xs())))
    case (Cons(h, t), Empty) => Some(((Some(h()), Option.empty[B]), (t(), empty[B])))
    case (Empty, Cons(x, xs)) => Some(((Option.empty[A], Some(x())), (empty[A], xs())))
    case _ => None
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.zipAll(s).takeWhile3(_._2.isDefined) forAll {
      case (left, right) => left == right
    }
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case Cons(h, t) => Some(cons(h(),t()), t())
    case Empty => None
  }.append(Stream(empty))

  def hasSubsequence[B >: A](s: Stream[B]): Boolean = tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B ): Stream[B] = foldRight((z, Stream(z)))(
    (a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    }
  )._2
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

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs(): Stream[Int] = {
    def createSequence(first: Int, second: Int): Stream[Int] = {
      val third = first + second
      val fourth = second + third
      cons(first, cons(second, createSequence(third, fourth)))
    }

    createSequence(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((value, state)) => cons(value, unfold(state)(f))
    case None => Empty
  }

  def fibs2(): Stream[Int] = unfold((0, 1)) {
    case (first, second) => Some(first, (second, first + second))
  }

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x, x + 1))

  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some(a, a))

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
    println(testStream.flatMap(i => if (i == 2) Stream(i) else empty).toList)
    println(testStream.flatMap(_ => empty).toList)

    printBlueLine("find")
    println(testStream.find(_ == 3))

    printBlueLine("constant")
    println(constant("blah").take(3).toList)

    printBlueLine("from")
    println(from(5).take(5).toList)
    println(from(345632).take(10).toList)

    printBlueLine("fibs")
    println(fibs().take(10).toList)

    printBlueLine("unfold")
    println(unfold(true)(_ => Some("Woo", true)).take(5).toList)
    println(unfold(1)(state => Some(state, state + 1)).take(5).toList)

    printBlueLine("fibs2")
    println(fibs2().take(10).toList)

    printBlueLine("from2")
    println(from2(5).take(5).toList)

    printBlueLine("constant2")
    println(constant2("blah").take(3).toList)

    printBlueLine("ones")
    val ones: Stream[Int] = constant2(1)
    println(ones.take(5).toList)

    printBlueLine("map2")
    println(testStream.map2(x => s"$x!").toList)

    printBlueLine("take3")
    println(testStream.take3(3).toList)
    println(testStream.take3(10).toList)

    printBlueLine("takeWhile3")
    println(testStream.takeWhile3(x => x < 3).toList)
    println(testStream.takeWhile3(x => x < 10).toList)

    printBlueLine("zipWith")
    println(testStream.zipWith(Stream(5, 4, 3, 2, 1))(_ * _).toList)

    printBlueLine("zipAll")
    val longStream: Stream[Int] = Stream(10, 9, 8, 7, 6, 5, 4, 3, 2, 1)
    println(testStream.zipAll(longStream).toList)
    println(longStream.zipAll(testStream).toList)

    printBlueLine("startsWith")
    println(testStream startsWith Stream(1, 2))

    printBlueLine("tails")
    println(testStream.tails.toList.map(_.toList))

    printBlueLine("scanRight")
    println(Stream(1,2,3).scanRight(0)(_ + _).toList)

  }
}
