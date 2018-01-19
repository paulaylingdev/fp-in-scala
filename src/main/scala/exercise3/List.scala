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
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile2(xs)(f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]): Int =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)(_ * _)

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, y) => 1 + y)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)

  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)

  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((x, _) => x + 1)

  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])((x: List[A], y: A) => Cons(y, x))

  def foldLeftAsRight[A, B](as: List[A], z: B)(f: (B, A) => B): B = ???

  def foldRightAsLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b,a) => f(a,b))

  def append[A](as: List[A], a: A): List[A] =
    foldRight(as, Cons(a, Nil))((x: A, xs: List[A]) => Cons(x,xs))

  def appendLeft[A](as: List[A], a: A): List[A] =
    foldLeft(reverse(as), Cons(a, Nil))((xs: List[A], x: A) => Cons(x,xs))

  def concat[A](as: List[List[A]]): List[A] =
    foldLeft(as, Nil:List[A])((x: List[A], xs: List[A]) => foldLeft(xs, x)((xs, x) => appendLeft(xs, x)))

  def addOne(as: List[Int]): List[Int] =
    foldLeft(reverse(as), Nil:List[Int])((xs, x) => Cons(x + 1, xs))

  def transformElements[A](as: List[A])(f: A => A): List[A] =
    foldLeft(reverse(as), Nil:List[A])((xs, x) => Cons(f(x), xs))

  def doubleToString(as: List[Double]): List[String] =
    foldLeft(as, Nil:List[String])((xs, x) => append(xs, x.toString))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil:List[B])((xs, x) => append(xs, f(x)))

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

    //Ex 3.6
    val initList = init(List(1,2,3,4))
    println(initList)

    val dropWhile2List = dropWhile2(List(1,2,3,4,5))(x => x < 4)
    println(dropWhile2List)

    //Ex 3.7 No. foldRight is evaluated before f is called

    //Ex 3.8
    val threepointeight = foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
    println(threepointeight)

    //Ex. 3.9
    val lengthList = length(List(1,2,3,4,5))
    println("length of list is " + lengthList)

    //Ex 3.10 + 3.11
    val sumLeftList = sumLeft(List(1,2,3))
    println(sumLeftList)
    val productLeftList = productLeft(List(1,2,3))
    println(productLeftList)
    val lengthLeftList = lengthLeft(List(1,2,3))
    println(lengthLeftList)

    //Ex 3.12

    val reverseList = reverse(List(1,2,3))
    println(reverseList)

    //Ex 3.14
    val foldRightAppend = append(List(1,2,3), 4)
    println(foldRightAppend)
    val foldLeftAppend = appendLeft(List(1,2,3), 4)
    println(foldLeftAppend)

    //Ex 3.15
    val concatLists = concat(List(List(1,2), List(3,4), List(5,6)))
    println(concatLists)

    //Ex 3.16
    val addOneList = addOne(List(1,2,3,4,5))
    println(addOneList)

    val addOneList2 = transformElements(List(1,2,3,4,5))(x => x + 1)
    println(addOneList2)

    //Ex 3.17
    val doubleToStringList = doubleToString(List(1.0,2.0,3.0,4.0,5.0))
    println(doubleToStringList)

    //Ex 3.18
    val mapExample = map(List(1,2,3,4,5))(x => "a" + x.toString)
    println(mapExample)


  }
}

