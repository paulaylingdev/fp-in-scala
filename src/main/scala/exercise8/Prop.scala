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


}