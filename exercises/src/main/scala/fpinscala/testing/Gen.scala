package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

//trait Prop {
  //def check: Either[(FailedCase, SuccessCount),SuccessCount]
  /*def &&(p: Prop): Prop = new Prop {
    override def check = this.check && p.check
  }*/
//}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result { def isFalsified = false }
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}

case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) match {
      case Passed => p.run(max, n, rng)
      case x => x
    }
  }

  def ||(p: Prop): Prop = Prop {
    (max, n, rng) => this.run(max, n, rng) match {
      case Passed => Passed
      case _ => p.run(max, n, rng)
    }
  }
}

object Prop {
  type MaxSize = Int
  type TestCases = Int
  type SuccessCount = Int
  type FailedCase = String

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def runTest[A](a: A, testNumber: Int)(f: A => Boolean): Result = {
    try {
      if(f(a)) Passed else Falsified(a.toString, testNumber)
    } catch {
      case e: Exception => Falsified(buildMsg(a, e), testNumber)
    }
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(i => g.forSize(i))(f)

  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop {
    (_,n,rng) => {
      val tests = randomStream(gen)(rng).zip(Stream.from(0)).take(n)
      val executedTests = tests.map { case (a, testNumber) => runTest(a, testNumber)(f) }
      executedTests.find(_.isFalsified).getOrElse(Passed)
    }
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) => {
      val casesPerSize = (n + (max - 1)) / max
      val minCases = math.min(n, max)
      val props: Stream[Prop] = Stream.from(0).take(minCases).map(i => forAll(g(i))(f))
      val prop: Prop = props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)

      prop.run(max,n,rng)
    }
  }

  def run(
    p: Prop,
    maxSize: Int = 100,
    testCases: Int = 100,
    rng: RNG = RNG.Simple(System.currentTimeMillis)
  ): Unit = p.run(maxSize, testCases, rng) match {
    case Falsified(msg, n) =>
      println(s"! Falsified after $n passed tests:\n $msg")
    case Passed =>
      println(s"+ OK, passed $testCases tests.")
  }
}

object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def int: Gen[Int] = Gen(State(RNG.int))

  def boolean: Gen[Boolean] = Gen(State(RNG.boolean))

  def double: Gen[Double] = Gen(State(RNG.double))

  def tuple2[A](g: Gen[A]): Gen[(A, A)] = map2(g, g)((a, b) => (a, b))

  def tuple3[A](g: Gen[A]): Gen[(A, A, A)] = for {
    x <- g
    y <- g
    z <- g
  } yield (x, y, z)

  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => listOfN(n.max(1), g))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.range(0, n).map(_ => g.sample)))

  def asOption[A](g: Gen[A]): Gen[Option[A]] =
    map2(g, boolean)((v, b) => if (b) Some(v) else None)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  def map2[A, B, C](ga: Gen[A], gb: Gen[B])(f: (A, B) => C): Gen[C] = for {
    a <- ga
    b <- gb
  } yield f(a, b)

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if(b) g1 else g2)

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    // The perctange ratio where g1 starts
    val g1Threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen.double.flatMap(d => if(d < g1Threshold) g1._1 else g2._1)
  }

  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(n => listOfN(n, g))
}

/*trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}*/

case class Gen[+A](sample: State[RNG,A]) {
  def map[B](f: A => B): Gen[B] = {
    Gen(sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.flatMap(a => f(a).sample))
  }

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  def unsized: SGen[A] = SGen((i: Int) => this)
}

case class SGen[+A](forSize: Int => Gen[A]) {
  def map[B](f: A => B): SGen[B] = SGen(i => forSize(i).map(f))

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(i => forSize(i).flatMap(a => f(a).forSize(i)))
}

object GenExamples {
  def smallInt: Unit = {
    val smallInt = Gen.choose(-10, 10)
    val maxProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    Prop.run(maxProp)
  }

  val sortedList: Unit = {
    val smallInt = Gen.choose(-10, 10)
    val sortedProp = Prop.forAll(Gen.listOf1(smallInt)) { ns =>
      val min = ns.min
      val max = ns.max
      val sorted = ns.sorted
      sorted.head == min && sorted.last == max
    }
    Prop.run(sortedProp)
  }
}
