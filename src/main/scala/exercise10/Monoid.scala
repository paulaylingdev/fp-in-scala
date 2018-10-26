package exercise10

import exercise10.Laws.{associative, identity}
import exercise8.Prop._
import exercise8.{Gen, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A

}

object Laws {
  def associative[A](m: Monoid[A])(a: A, b: A, c: A): Boolean =
    m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)

  def identity[A](m: Monoid[A])(a: A): Boolean =
    m.op(a, m.zero) == a
}

object Monoids {
  val stringMonoid: Monoid[String] = new Monoid[String] {
    def op(a1: String, a2: String): String = a1 + a2

    def zero = ""
  }

  def listMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]): List[A] = a1 ++ a2

    def zero: List[A] = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 + a2

    def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    def op(a1: Int, a2: Int): Int = a1 * a2

    def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2

    def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    def op(a1: A => A, a2: A => A): A => A = a1 compose a2

    def zero: A => A = (a: A) => a
  }

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    forAll(
      for {
        x <- gen
        y <- gen
        z <- gen
      } yield (x, y, z))(p =>
        associative(m)(p._1, p._2, p._3)) &&
    forAll(gen)(a => identity(m)(a))

  def concatenate[A](as: List[A], m: Monoid[A]): A = as.foldLeft(m.zero)(m.op)

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = concatenate(as.map(f), m)
}
