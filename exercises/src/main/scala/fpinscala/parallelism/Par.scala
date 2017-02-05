package fpinscala.parallelism

import java.util.concurrent._
import language.implicitConversions

object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  // `unit` is represented as a function that returns a `UnitFuture`, which is a simple implementation of `Future`
  // that just wraps a constant value. It doesn't use the `ExecutorService` at all. It's always done and can't be cancelled.
  // Its `get` method simply returns the value that we gave it.
  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone: Boolean = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled: Boolean = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  // `map2` doesn't evaluate the call to `f` in a separate logical thread, in accord with our design choice of having `fork`
  // be the sole function in the API for controlling parallelism. We can always do `fork(map2(a,b)(f))` if we want the evaluation
  // of `f` to occur in a separate thread.
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)

      // This implementation of `map2` does _not_ respect timeouts, and eagerly waits for the returned futures. This means that even if you have
      // passed in "forked" arguments, using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`. In order to respect timeouts,
      // we'd need a new `Future` implementation that records the amount of time spent evaluating `af`, then subtracts that time from the available
      // time allocated for evaluating `bf`.
      UnitFuture(f(af.get, bf.get))
    }

  // This is the simplest and most natural implementation of `fork`, but there are some problems with it--for one, the outer `Callable` will block waiting
  // for the "inner" task to complete. Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`, this implies
  // that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice. This is a symptom of a more serious
  // problem with the implementation, and we will discuss this later in the chapter.
  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def flatten[A](a: Par[Par[A]]): Par[A] =
    (es: ExecutorService) => {
      a(es).get()(es)
    }

  def flatMap[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    (es: ExecutorService) => {
      val a: A = pa(es).get
      f(a)(es)
    }

  def flatMapByFlatten[A,B](pa: Par[A])(f: A => Par[B]): Par[B] =
    (es: ExecutorService) => Par.flatten(Par.map(pa)(f))(es)

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choice2[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val n = Par.map(cond)(c => if(c) 0 else 1)
    choiceN(n)(List(t, f))
  }

  def choiceN[A](nPar: Par[Int])(choices: List[Par[A]]): Par[A] =
    Par.flatMap(nPar)(n => choices(n))

  def choiceMap[K,V](key: Par[K])(choices: Map[K, Par[V]]): Par[V] =
    Par.flatMap(key)(k => choices(k))

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A, B](f: A => B): A => Par[B] =
    (a: A) => lazyUnit(f(a))

  def asyncF1[A, B](f: A => B): A => Par[B] =
    (a: A) => fork(map(unit(a))(f))

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    val initialState = unit(Nil : List[A])
    ps.foldRight(initialState)((s, par) => map2(s, par)(_ :: _))
  }

  /* Gives us infix syntax for `Par`. */
  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
  }
}

object Examples {
  import Par._

  // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient
  // `splitAt` method for dividing them into two parts at a particular index.
  def sum(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1) {
      // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
      ints.headOption.getOrElse(0)
    } else {
      val (l, r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
      sum(l) + sum(r) // Recursively sum both halves and add the results together.
    }

}
