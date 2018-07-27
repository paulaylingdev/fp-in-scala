package exercise8

import exercise8.Prop._
import exercise5.Stream
import main.scala.exercise6.{Main, RNG, State}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case class Prop(run: (TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop((testCases, rng) => {
      run(testCases, rng) match {
        case Passed => p.run(testCases, rng)
        case anythingElse => anythingElse
      }
    })
  }
  def ||(p: Prop): Prop = {
    Prop((testCases, rng) => {
      run(testCases, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(testCases, rng)
        case anythingElse => anythingElse
      }
    })
  }

  def tag(msg: String): Prop = Prop {
    (testCases, rng) => run(testCases, rng) match {
      case Falsified(e, c) => Falsified(s"$msg \n $e", c)
      case anythingElse => anythingElse
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zipAll(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a.get)) Passed else Falsified(a.toString, i.get)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i.get)}
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"
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

  def unsized: SGen[A] = SGen(_ => this)
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

case class SGen[A](forSize: Int => Gen[A])