package main.scala.exercise3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def main(args: Array[String]): Unit = {

    val singleLeafTree = Leaf(1)
    val exampleTree = Branch(Leaf(1), Leaf(2))

    println(size(singleLeafTree))
    println(size(exampleTree))

  }
}
