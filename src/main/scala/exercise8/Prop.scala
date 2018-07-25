package exercise8

import main.scala.exercise6.{Main, RNG, State}

trait Prop {
  def check: Boolean

  def &&(p: Prop): Prop = new Prop {
    override def check: Boolean = Prop.this.check && p.check
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[A](sample: State[RNG, A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(input => f(input).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => {
      Gen(State.sequence(List.fill(n)(this.sample)))
    })
  }
}

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(Main.nonNegativeInt).map(generatedInt => start + generatedInt % (stopExclusive - start)))
  }

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] = {
    Gen(State(Main.nonNegativeInt).map(i => (i % 2) == 0))
  }

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = {
    Gen(State.sequence(List.fill(n)(g.sample)))
  }

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    Gen.boolean.flatMap(chooser => {
      if (chooser) g1 else g2
    })
  }

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val max = (g1._2 + g2._2) * 100
    Gen.choose(0, max.toInt + 1).flatMap(value => {
      val percent = value.toDouble / 100
      if (percent <= g1._2)
        g1._1
      else
        g2._1
    })
  }
}