package main.scala.exercise7

/*
Exercise 7.1
map2 signature
 */
trait Par[A] {
  def map2[B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]
}