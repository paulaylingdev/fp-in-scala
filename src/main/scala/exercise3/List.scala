package main.scala.exercise3

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](head: A, as: List[A]): List[A] = as match {
    case Cons(_, xs) => Cons(head, xs)
    case Nil => Cons(head, Nil)
  }

//  def drop[A](l: List[A], n: Int): List[A] = l match {
//    case Nil => Nil
//    case Cons(_, xs) => if (n <= 1) xs else drop(xs, n-1)
//  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else drop(tail(l), n-1)

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
  }

  def main(args: Array[String]): Unit = {
    val ex1: List[Double] = Nil
    val ex2: List[Int] = Cons(1, Nil)
    val ex3: List[String] = Cons("a", Cons("b", Nil))

    //Answer for 3.1 is x = 3 (3rd pattern)
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println("x is " + x)

    //Ex 3.2
    val tailList = tail(List(1,2,3,4,5))
    println(tailList)

    val NilList = tail(Nil)
    println(NilList)

    //Ex 3.3
    val headList = setHead(9,List(1,2,3,4,5))
    println(headList)

    println(setHead(5,NilList))

    //Ex 3.4
    val dropList = drop(List(1,2,3,4,5), 3)
    println(dropList)

    //Ex 3.5
    //Drop while head is 1
    val dropWhileList = dropWhile(List(1,1,1,1,2,3,4,5), (x: Int) => x == 1)
    println(dropWhileList)
  }
}

