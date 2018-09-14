package exercise8

import java.util.concurrent.Executors

import exercise5.Stream
import exercise7.Par.Par
import exercise8.Gen.{choose, unit, weighted}
import exercise8.Prop._
import main.scala.exercise6.{Main, RNG, SimpleRNG, State}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

case object Proved extends Result {
  override def isFalsified: Boolean = false
}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {
  def &&(p: Prop): Prop = {
    Prop((max, testCases, rng) => {
      run(max, testCases, rng) match {
        case Passed => p.run(max, testCases, rng)
        case anythingElse => anythingElse
      }
    })
  }

  def ||(p: Prop): Prop = {
    Prop((max, testCases, rng) => {
      run(max, testCases, rng) match {
        case Falsified(msg, _) => p.tag(msg).run(max, testCases, rng)
        case anythingElse => anythingElse
      }
    })
  }

  def tag(msg: String): Prop = Prop {
    (max, testCases, rng) =>
      run(max, testCases, rng) match {
        case Falsified(e, c) => Falsified(s"$msg \n $e", c)
        case anythingElse => anythingElse
      }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis())): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(failure, successes) =>
        println(s"! Falsified after $successes passed tests:\n $failure")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property.")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Proved else Falsified("()", 0)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (_, n, rng) =>
      randomStream(as)(rng).zipAll(Stream.from(0)).take(n).map {
        case (a, i) => try {
          if (f(a.get)) Passed else Falsified(a.toString, i.get)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i.get)
        }
      }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max + 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop = {
    val S = weighted(choose(1, 4).map(Executors.newFixedThreadPool) -> 0.75,
      unit(Executors.newCachedThreadPool) -> 0.25)
    forAll(S ** g) { case s ** a => f(a)(s).get }
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

  def map[B](f: A => B): Gen[B] = {
    flatMap(input => Gen.unit(f(input)))
  }

  def map2[B, C](b: Gen[B])(f: (A, B) => C): Gen[C] = {
    for {
      input <- this
      input2 <- b
    } yield f(input, input2)
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] = {
    size.flatMap(n => {
      Gen(State.sequence(List.fill(n)(this.sample)))
    })
  }

  def unsized: SGen[A] = SGen(_ => this)

  def **[B](g2: Gen[B]): Gen[(A, B)] = (this map2 g2) ((_, _))
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

  def listOf[A](g: Gen[A]): SGen[List[A]] = {
    SGen(size => {
      g.listOfN(Gen.unit(size))
    })
  }

  def listOf1[A](g: Gen[A]): SGen[List[A]] = {
    SGen(size => {
      g.listOfN(Gen.unit(size max 1))
    })
  }
}

object ** {
  def unapply[A, B](arg: (A, B)): Option[(A, B)] = Option(arg)
}

case class SGen[A](forSize: Int => Gen[A]) {
  def apply(n: Int): Gen[A] = forSize(n)

  def map[B](f: A => B): SGen[B] = SGen {
    forSize(_) map f
  }

  def flatMap[B](f: A => SGen[B]): SGen[B] = {
    val forSize2: Int => Gen[B] = n => {
      forSize(n) flatMap {
        f(_).forSize(n)
      }
    }
    SGen(forSize2)
  }

  def **[B](s2: SGen[B]): SGen[(A, B)] = SGen(n => apply(n) ** s2(n))

}