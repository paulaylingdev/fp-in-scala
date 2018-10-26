package exercise7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

/*
Exercise 7.1
map2 signature

trait Par[A] {
  def map2[B, C](a: => Par[A], b: => Par[B])(f: (A, B) => C): Par[C]
}
*/

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true

    def get(timeout: Long, units: TimeUnit): A = get

    def isCancelled = false

    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      Map2Future(af, bf, f)
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = {
    a(s)
  }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  case class Map2Future[A, B, C](a: Future[A], b: Future[B], f: (A, B) => C) extends Future[C] {
    @volatile var cache: Option[C] = None

    def isDone: Boolean = cache.isDefined

    def isCancelled: Boolean = a.isCancelled || b.isCancelled

    def cancel(evenIfRunning: Boolean): Boolean =
      a.cancel(evenIfRunning) || b.cancel(evenIfRunning)

    def get: C = compute(Long.MaxValue)

    def get(timeout: Long, units: TimeUnit): C =
      compute(TimeUnit.NANOSECONDS.convert(timeout, units))

    private def compute(timeoutInNanos: Long): C = cache match {
      case Some(c) => c
      case None =>
        val start = System.nanoTime
        val ar = a.get(timeoutInNanos, TimeUnit.NANOSECONDS)
        val stop = System.nanoTime
        val aTime = stop - start
        val br = b.get(timeoutInNanos - aTime, TimeUnit.NANOSECONDS)
        val ret = f(ar, br)
        cache = Some(ret)
        ret
    }
  }

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.reverse.foldLeft(unit(List.empty[A]))(
      (accumulation: Par[List[A]], p: Par[A]) =>
        map2(p, accumulation)(_ :: _)
    )
  }

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pars: List[Par[List[A]]] =
      as.map(
        asyncF(
          (a: A) => if (f(a)) List(a) else List()
        )
      )
    map(sequence(pars))(_.flatten)
  }

  def sum[A](as: IndexedSeq[A], default: A)(f: (A, A) => A): Par[A] = {
    if (as.length <= 1)
      unit(as.headOption getOrElse default)
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(fork(sum(l, default)(f)), fork(sum(r, default)(f)))(f)
    }
  }

  def countParagraphs(paragraphs: List[String]): Par[Int] = {
    es => {
      val pars: Par[List[Int]] = parMap(paragraphs)(s => s.split(" ").length)
      val is: List[Int] = run(es)(pars).get()
      run(es)(sum(is.toIndexedSeq, 0)(_ + _))
    }
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    es => {
      val choice: Int = run(es)(n).get
      choices(choice)(es)
    }
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map[Boolean, Int](cond)(bool => if (bool) 0 else 1))(List(t, f))
  }

  def choiceMap[K, V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] = {
    es => {
      choices(key(es).get)(es)
    }
  }

  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] = {
    es => choices(pa(es).get)(es)
  }

  def chooserChoiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)(choices(_))
  }

  def chooserChoice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    chooser(cond)(if (_) t else f)
  }

  def join[A](a: Par[Par[A]]): Par[A] = {
    es => a(es).get()(es)
  }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = {
    join(map(a)(f(_)))
  }

  def joinFlatMap[A](a: Par[Par[A]]): Par[A] = {
    flatMap(a)((input: Par[A]) => input)
  }

  def map2FlatMap[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = {
    flatMap(a)((aa: A) =>
      flatMap(b)((bb: B) => unit(f(aa, bb)))
    )
  }

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = map2(p, p2)(_ == _)

  def sequenceBalanced[A](as: IndexedSeq[Par[A]]): Par[IndexedSeq[A]] = fork {
    if (as.isEmpty) unit(Vector())
    else if (as.length == 1) map(as.head)(a => Vector(a))
    else {
      val (l, r) = as.splitAt(as.length / 2)
      map2(sequenceBalanced(l), sequenceBalanced(r))(_ ++ _)
    }
  }

  def parMap[A, B](as: IndexedSeq[A])(f: A => B): Par[IndexedSeq[B]] =
    sequenceBalanced(as.map(asyncF(f)))
}
