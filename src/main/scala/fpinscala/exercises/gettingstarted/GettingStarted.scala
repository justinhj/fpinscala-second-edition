package fpinscala.exercises.gettingstarted

// A comment!
/* Another comment */
/** A documentation comment */
object MyProgram:
  def abs(n: Int): Int =
    if n < 0 then -n
    else n

  private def formatAbs(x: Int) =
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))

  @main def printAbs: Unit =
    println(formatAbs(-42))

  // A definition of factorial, using a local, tail recursive function
  def factorial(n: Int): Int =
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if n <= 0 then acc
      else go(n-1, n*acc)

    go(n, 1)

  // Another implementation of `factorial`, this time with a `while` loop
  def factorial2(n: Int): Int =
    var acc = 1
    var i = n
    while i > 0 do { acc *= i; i -= 1 }
    acc

  // Exercise 1: Write a function to compute the nth fibonacci number

  def fib(n: Int): Int = 
    def go(n: Int, a: Int, b: Int): Int =
      if n == 0 then a
      else go(n - 1, b, a + b)
    go(n, 0, 1)

  // This definition and `formatAbs` are very similar..
  private def formatFactorial(n: Int) =
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))

  // We can generalize `formatAbs` and `formatFactorial` to
  // accept a _function_ as a parameter
  def formatResult(name: String, n: Int, f: Int => Int) =
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))

object FormatAbsAndFactorial:

  import MyProgram.*

  // Now we can use our general `formatResult` function
  // with both `abs` and `factorial`
  @main def printAbsAndFactorial: Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))

object TestFib:

  import MyProgram.*

  // test implementation of `fib`
  @main def printFib: Unit =
    println("Expected: 0, 1, 1, 2, 3, 5, 8")
    println("Actual:   %d, %d, %d, %d, %d, %d, %d".format(fib(0), fib(1), fib(2), fib(3), fib(4), fib(5), fib(6)))

// Functions get passed around so often in FP that it's
// convenient to have syntax for constructing a function
// *without* having to give it a name
object AnonymousFunctions:

  import MyProgram.*

  // Some examples of anonymous functions:
  @main def printAnonymousFunctions: Unit =
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("increment", 7, (x: Int) => x + 1))
    println(formatResult("increment2", 7, (x) => x + 1))
    println(formatResult("increment3", 7, x => x + 1))
    println(formatResult("increment4", 7, _ + 1))
    println(formatResult("increment5", 7, x => { val r = x + 1; r }))

object MonomorphicBinarySearch:


  // First, a findFirst, specialized to `String`.
  // Ideally, we could generalize this to work for any `Array` type.
  def findFirst(ss: Array[String], key: String): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      // If `n` is past the end of the array, return `-1`
      // indicating the key doesn't exist in the array.
      if n >= ss.length then -1
      // `ss(n)` extracts the n'th element of the array `ss`.
      // If the element at `n` is equal to the key, return `n`
      // indicating that the element appears in the array at that index.
      else if ss(n) == key then n
      else loop(n + 1) // Otherwise increment `n` and keep looking.
    // Start the loop at the first element of the array.
    loop(0)

object PolymorphicFunctions:

  // Here's a polymorphic version of `findFirst`, parameterized on
  // a function for testing whether an `A` is the element we want to find.
  // Instead of hard-coding `String`, we take a type `A` as a parameter.
  // And instead of hard-coding an equality check for a given key,
  // we take a function with which to test each element of the array.
  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @annotation.tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      // If the function `p` matches the current element,
      // we've found a match and we return its index in the array.
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  // Exercise 2: Implement a polymorphic function to check whether
  // an `Array[A]` is sorted
  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = 
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if n >= as.length - 1 then true
      else if gt(as(n), as(n + 1)) then false
      else loop(n + 1)

    loop(0)


  // Polymorphic functions are often so constrained by their type
  // that they only have one implementation! Here's an example:

  def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // Exercise 3: Implement `curry`.

  // Note that `=>` associates to the right, so we could
  // write the return type as `A => B => C`
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => partial1(a, f)

  // NB: The `Function2` trait has a `curried` method already

  // Exercise 4: Implement `uncurry`
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a,b) => f(a)(b)

  /*
  NB: There is a method on the `Function` object in the standard library,
  `Function.uncurried` that you can use for uncurrying.

  Note that we can go back and forth between the two forms. We can curry
  and uncurry and the two forms are in some sense "the same". In FP jargon,
  we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
  a term we inherit from category theory.
  */

  // Exercise 5: Implement `compose`

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))

