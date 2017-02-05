package fpinscala.laziness

import Stream._
trait Stream[+A] {

  // The arrow `=>` in front of the argument type `B` means that the function
  // `f` takes its second argument by name and may choose not to evaluate it.
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

  // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList: List[A] = foldRight(Nil : List[A])((v, list) => v :: list)

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def take2(num: Int): Stream[A] = unfold((this, num)) {
    case (Cons(h, t), n) if n > 0 => Some((h(), (t(), n - 1)))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(p(h)) cons(h,t) else empty)

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((v, b) => p(v) && b)

  def headOption: Option[A] = foldRight(None : Option[A])((h, _) => Some(h))

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case Empty => None
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(p(h)) cons(h, t) else t)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def zip[B](s: Stream[B]): Stream[(A, B)] = zipWith(s)((a, b) => (a, b))

  def zipWith[B, C](s: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), ((t1(), t2()))))
    case _ => None
  }

  def zipAll[B](s: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s)((a, b) => (a, b))

  def zipWithAll[B, C](s: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold((this, s)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some((f(Some(h1()), None), (t1(), Empty)))
      case (Empty, Cons(h2, t2)) => Some((f(None, Some(h2())), (Empty, t2())))
      case _ => None
    }

  def startsWith[B](s: Stream[B]): Boolean =
    zipAll(s).takeWhile(t => !t._2.isEmpty).forAll(t => t._1 == t._2)
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  val ones2: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))
  def constant2[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))
  def from2(n: Int): Stream[Int] = unfold(n)(n => Some((n, n + 1)))

  val fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, go(b, a + b))
    }

    go(0, 1)
  }
  val fibs2: Stream[Int] = unfold((0, 1)) {
    case (a, b) => Some((a, (b, a + b)))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream.empty
  }
}
