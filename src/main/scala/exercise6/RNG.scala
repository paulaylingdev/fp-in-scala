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

  println(nonNegativeInt(SimpleRNG(5)))
}
