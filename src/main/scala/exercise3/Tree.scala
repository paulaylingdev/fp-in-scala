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

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def foldSize[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((left, right) => 1 + left + right)

  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(a => a)((left, right) => left.max(right))

  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((left, right) => 1 + left.max(right))

  def foldMap[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((left, right) => Branch(left, right))

  def main(args: Array[String]): Unit = {

    /* singleLeafTree
          | 1 |
     */
    val singleLeafTree = Leaf(1)

    /* singleBranchTree
           _  |  _
       | 1 |    | 2 |
     */
    val singleBranchTree = Branch(Leaf(1), Leaf(2))

    /* treeInBook
             _     |     _
       _   |   _       _   |   _
    | a |    | b |   | c |   | d |
     */
    val treeInBook = Branch(Branch(Leaf("a"), Leaf("b")), Branch(Leaf("c"), Leaf("d")))

    /* intTree
             _     |     _
       _   |   _       _   |   _
    | 1 |    | 2 |   | 3 |   | 4 |
     */
    val intTree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))

    /* leftSidedTree
                    _     |     _
              _   |   _          | 1 |
        _   |   _      | 2 |
     | 3 |   | 4 |
     */
    val leftSidedTree = Branch(Branch(Branch(Leaf(3), Leaf(4)), Leaf(2)), Leaf(1))

    def printBlueln[A](s: A): Unit =
      println(Console.BLUE + s + Console.RESET)

    printBlueln("Size")
    println(size(singleLeafTree))
    println(size(singleBranchTree))
    println(size(treeInBook))

    printBlueln("Maximum")
    println(maximum(singleLeafTree))
    println(maximum(singleBranchTree))
    println(maximum(intTree))

    printBlueln("Depth")
    println(depth(singleLeafTree))
    println(depth(singleBranchTree))
    println(depth(treeInBook))
    println(depth(leftSidedTree))

    printBlueln("Map")
    println(map(singleLeafTree)(_ + 1))
    println(map(singleBranchTree)(_ * 2))
    println(map(treeInBook)(_.toUpperCase))
    println(map(treeInBook)(_ == "a"))

    printBlueln("### Fold Methods ###")

    printBlueln("Size")
    println(foldSize(singleLeafTree))
    println(foldSize(singleBranchTree))
    println(foldSize(treeInBook))

    printBlueln("Maximum")
    println(foldMaximum(singleLeafTree))
    println(foldMaximum(singleBranchTree))
    println(foldMaximum(intTree))

    printBlueln("Depth")
    println(foldDepth(singleLeafTree))
    println(foldDepth(singleBranchTree))
    println(foldDepth(treeInBook))
    println(foldDepth(leftSidedTree))

    printBlueln("Map")
    println(foldMap(singleLeafTree)(_ + 1))
    println(foldMap(singleBranchTree)(_ * 2))
    println(foldMap(treeInBook)(_.toUpperCase))
    println(foldMap(treeInBook)(_ == "a"))
  }
}
