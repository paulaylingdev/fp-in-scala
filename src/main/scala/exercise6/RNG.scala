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


  println(nonNegativeInt(SimpleRNG(5)))

  println(double(SimpleRNG(100)))

  println(intDouble(SimpleRNG(5)))

  println(doubleInt(SimpleRNG(5)))

  println(double3(SimpleRNG(5)))
}
