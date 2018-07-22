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

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State {
    (state: S) => {
      val (a, s2) = this.run(state)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }
  }

  def flatMap[B](g: A => State[S, B]): State[S, B] = State {
    (state: S) => {
      val (a, s2) = run(state)
      g(a).run(s2)
    }
  }
}

object State {
  def unit[A, S](a: A): State[S, A] = State {
    (state: S) => (a, state)
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[A, S](fs: List[State[S, A]]): State[S, List[A]] =
    fs.reverse.foldLeft(unit[List[A], S](List.empty[A]))(
      (list: State[S, List[A]], item: State[S, A]) => {
        item.map2(list)(_ :: _)
      }
    )
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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  val randIntDouble: Rand[(Int, Double)] = both(int, newDouble)

  val randDoubleInt: Rand[(Double, Int)] = both(newDouble, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.reverse.foldLeft(unit(List.empty[A]))(
      (list: Rand[List[A]], item: Rand[A]) => {
        map2(item, list)(_ :: _)
      }
    )

  def seqInts(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def newDouble: Rand[Double] = map(nonNegativeInt)(_.toDouble / Int.MaxValue)

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(int)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0)
        unit(mod)
      else
        nonNegativeLessThan(n)
    })

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
    if (count <= 0)
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

  val x: Rand[Double] = newDouble
  println(x(SimpleRNG(5)))

  println(sequence(List(unit(1), unit(2), unit(3)))(SimpleRNG(1)))

  println(seqInts(10)(SimpleRNG(1)))

  println(nonNegativeLessThan(5)(SimpleRNG(2)))
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object FSA extends App {
  def update = (i: Input) => (s: Machine) =>
    (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs map (State.modify[Machine] _ compose update))
    s <- State.get
  } yield(s.coins, s.candies)

  println(simulateMachine(List(Coin, Turn)).run(Machine(locked = true, 10, 4)))
}
