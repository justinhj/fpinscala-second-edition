package fpinscala.exercises.state


trait RNG:
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.

object RNG:
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = r => r.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  def nonNegativeInt(rng: RNG): (Int, RNG) = 
    val (next, r2) = rng.nextInt
    if(next < 0)
      (-next, r2)
    else 
      (next, r2)

  def double(rng: RNG): (Double, RNG) =
    val (next, r2) = rng.nextInt
    (1.0 / next, r2)

  def intDouble(rng: RNG): ((Int,Double), RNG) =
    val (newInt, r2) = rng.nextInt
    val (newDouble, r3) = r2.nextInt
    ((newInt, 1.0 / newDouble), r3)

  def doubleInt(rng: RNG): ((Double,Int), RNG) =
    val (newDouble, r2) = rng.nextInt
    val (newInt, r3) = r2.nextInt
    ((1.0 / newDouble, newInt), r3)

  def double3(rng: RNG): ((Double,Double,Double), RNG) =
    val (newDouble1, r2) = rng.nextInt
    val (newDouble2, r3) = rng.nextInt
    val (newDouble3, r4) = rng.nextInt
    ((1.0 / newDouble1, 1.0 / newDouble2, 1.0 / newDouble3), r4)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    var r = rng
    val l = List.empty[Int]
    for(i <- 0 to count) yield
      val (next, r2) = r.nextInt
      r = r2
      l :+ next
    (l, r)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => 
      rs.foldLeft((List.empty[A], rng)){ 
        case ((newList, newRng), ra) =>
          val (newA, newR) = ra(newRng)
          (newA +: newList, newR)
      }

  def flatMap[A, B](r: Rand[A])(f: A => Rand[B]): Rand[B] = 
    rng =>
      val (a, rng2) = r(rng)
      val (b, rng3) = f(a)(rng2)
      (b, rng3)

  def mapViaFlatMap[A, B](r: Rand[A])(f: A => B): Rand[B] =
    flatMap(r)(a => unit(f(a)))

  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => map(rb)(b => f(a,b)))
    
opaque type State[S, +A] = S => (A, S)

object State:
  extension [S, A](underlying: State[S, A])
    def run(s: S): (A, S) = underlying(s)

    def map[B](f: A => B): State[S, B] =
      s =>
        val (a, s2) = underlying(s)
        (f(a), s2)

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      underlying.flatMap(a => sb.map(b => f(a,b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      s => 
        val (a,s2) = underlying(s)
        f(a)(s2)

  def apply[S, A](f: S => (A, S)): State[S, A] = f

enum Input:
  case Coin, Turn

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy:
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
