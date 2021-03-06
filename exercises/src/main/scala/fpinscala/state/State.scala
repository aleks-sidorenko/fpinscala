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
    flatMap(s) { a => unit(f(a)) }

  def positiveInt(rng: RNG): (Int, RNG) = { 
    val (a, rng2) = rng.nextInt
    if (a == 0 || a == Int.MinValue) positiveInt(rng2)
    else (a.abs, rng2)
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = { 
    val (a, rng2) = rng.nextInt
    if (a == Int.MinValue) (Int.MaxValue, rng2)
    else (a.abs, rng2)
  }

  def double(rng: RNG): (Double, RNG) = { 
    val (a, rng2) = rng.nextInt
    (a.toDouble/Int.MaxValue, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i, rng2) = int(rng)
    val (d, rng3) = double(rng2)
    (i -> d, rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case ((i, d), nrg2) => ((d -> i), nrg2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng1) = double(rng)
    val (d2, rng2) = double(rng1)
    val (d3, rng3) = double(rng2)
    (d1, d2, d3) -> rng3
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    sequence(List.fill(count)(int))(rng)
  }
    
    

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    flatMap(ra) { a => flatMap(rb) { b => unit(f(a, b)) } }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = 
    fs.foldRight(unit[List[A]](List.empty[A])) { (f, acc) => map2(f, acc)( _ :: _ ) }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = 
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
}

case class State[S,+A](run: S => (A, S)) {
  import State._

  def map[B](f: A => B): State[S, B] =
    flatMap { a => unit(f(a)) }
  
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    flatMap { a => sb.map { b => f(a, b) } }

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State[S, B]( { s => 
      val (a, as) = run(s)
      f(a).run(s)
    })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
   def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  type Rand[A] = State[RNG, A]
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
