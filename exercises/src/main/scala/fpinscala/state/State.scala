package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, nextState) = rng.nextInt
    val nextNum = num match {
      case Int.MinValue => Int.MaxValue
      case i: Int => math.abs(i)
    }

    (nextNum, nextState)
  }

  def boolean: Rand[Boolean] = map(int)(i => i % 2 == 0)

  def double(rng: RNG): (Double, RNG) = {
    val (num, nextRng) = nonNegativeInt(rng)
    val asDouble = num.toDouble / (Int.MaxValue.toDouble + 1)
    (asDouble, nextRng)
  }

  def double2(rng: RNG): Rand[Double] =
    map(nonNegativeInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng1) = nonNegativeInt(rng)
    val (d, rng2) = double(rng1)
    ((i, d), rng2)
  }

  def intDouble2: Rand[(Int, Double)] =
    both(int, double)

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (d, rng1) = double(rng)
    val (i, rng2) = nonNegativeInt(rng1)
    ((d, i), rng2)
  }

  def doubleInt2: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    val initialState = (Nil : List[Int], rng)
    val (list, rng2) = (0 to count).foldLeft(initialState)((acc, _) => {
      val (list, r) = acc
      val (num, nextRng) = r.nextInt
      (num :: list, nextRng)
    })

    (list.reverse, rng2)
  }

  def ints2(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(int))

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    (rng: RNG) => {
      val (v1, rng2) = ra(rng)
      val (v2, rng3) = rb(rng2)
      (f(v1, v2), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((a, b) => (a, b))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    val initialState = unit(List[A]())
    fs.foldRight(initialState)((f, acc) => map2(f, acc)((value, list) => value :: list))
  }

  def map_[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a, b)))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    (rng: RNG) => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State((s: S) => {
    val (a, s1) = run(s)
    f(a).run(s1)
  })
}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
    val initialState = unit[S, List[A]](List[A]())
    sas.foldRight(initialState)((sa, acc) => sa.map2(acc)((value, list) => value :: list))
  }

  def step(input: Input)(machine: Machine): Machine =
    (input, machine) match {
      // No candy: ignore all inputs
      case (_, Machine(_, 0, _)) => machine
      // Turning the knob on a locked machine does nothing.
      case (Turn, Machine(true, _, _)) => machine
      // Inserting a coin on an unlocked machine does nothing.
      case (Coin, Machine(false, _, _)) => machine
      // Inserting a coin on a locked machine unlocks it and increases the count.
      case (Coin, Machine(true, candy, coin)) => Machine(false, candy, coin + 1)
      // Turning the knob on an unlocked machine locks it and removes a candy
      case (Turn, Machine(false, candy, coin)) => Machine(true, candy - 1, coin)
    }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(input => State.modify(State.step(input))))
    finalMachine <- get
  } yield (finalMachine.candies, finalMachine.coins)
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)
