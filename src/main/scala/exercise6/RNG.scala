package main.scala.exercise6

trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Main extends App {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

  def newDouble(): Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (result, newRng) = rng.nextInt
    (if (result < 0) -(result + 1) else result, newRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (result, newRng) = nonNegativeInt(rng)
    (result.toDouble / Int.MaxValue, newRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (integer, newRng) = nonNegativeInt(rng)
    val (doubleFloat, newerRng) = double(newRng)
    ((integer, doubleFloat), newerRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((integer, double), newRng) = intDouble(rng)
    ((double, integer), newRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, finalRng) = double(rng2)
    ((d1, d2, d3), finalRng)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <=0)
      (List(), rng)
    else {
      val (integer, newRng) = rng.nextInt
      val (xs, newerRng) = ints(count - 1)(newRng)
      (integer :: xs, newerRng)
    }

  }


  println(nonNegativeInt(SimpleRNG(5)))

  println(double(SimpleRNG(100)))

  println(intDouble(SimpleRNG(5)))

  println(doubleInt(SimpleRNG(5)))

  println(double3(SimpleRNG(5)))

  println(ints(5)(SimpleRNG(5))._1)

  println(int(SimpleRNG(5)))

  val x: Rand[Double] = newDouble()
  println(x(SimpleRNG(5)))
}
