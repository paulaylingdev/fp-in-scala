package main.scala.exercise3

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => maximum(left).max(maximum(right))
  }

  def main(args: Array[String]): Unit = {

    val singleLeafTree = Leaf(1)
    val singleBranchTree = Branch(Leaf(1), Leaf(2))
    val treeInBook = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))
    val intTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    println("##Size")
    println(size(singleLeafTree))
    println(size(singleBranchTree))
    println(size(treeInBook))

    println("##Maximum")
    println(maximum(singleLeafTree))
    println(maximum(singleBranchTree))
    println(maximum(intTree))

  }
}
