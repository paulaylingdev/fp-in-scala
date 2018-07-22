package exercise8

import main.scala.exercise6.{Main, RNG, SimpleRNG, State}

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

case class Gen[A](sample: State[RNG, A])

object Gen {
  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State(Main.nonNegativeInt).map {
      generatedInt: Int => generatedInt + start % (stopExclusive - start)
    })

  }
}