package exercise7
import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

import scala.collection.immutable

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
    val pars: Par[List[Int]] = parMap(paragraphs)(s => s.split(" ").length)
    map(pars)(_.sum)
//    map[List[Int], Par[Int]](pars)(a => sum(a.toIndexedSeq, 0)(_ + _))
  }
}
