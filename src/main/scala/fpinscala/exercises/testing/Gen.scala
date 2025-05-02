package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import Result.*
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

opaque type Gen[+A] = State[RNG, A]

opaque type TestCases = Int
object TestCases:
  extension (x: TestCases) def toInt: Int = x
  def fromInt(x: Int) : TestCases = x

opaque type Prop = (TestCases, RNG) => Result

object Prop:
  opaque type FailedCase = String
  opaque type SuccessCount = Int

  def failedCase(s: String): FailedCase = s
  def successCount(n: Int): SuccessCount = n
 
  extension (self: Prop)
    def &&(that: Prop) : Prop = 
      (tc, rng) => 
        self(tc, rng) match 
        case F @ Falsified(msg, c) => F
        case Passed => 
          that(tc, rng)

    def ||(that: Prop): Prop =
      (tc, rng) => 
        self(tc, rng) match 
        case F @ Falsified(msg, c) => 
          that(c, rng)
        case Passed => 
          Passed

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop =
    (n, rng) =>
      randomLazyList(as)(rng)
        .zip(LazyList.from(0))
        .take(n)
        .map:
          case (a, i) =>
            try
              if f(a) then Passed
              else Falsified(failedCase(a.toString), successCount(i))
            catch
              case e: Exception =>
                Falsified(failedCase(buildMsg(a, e)), successCount(i))
        .find(_.isFalsified)
        .getOrElse(Passed)
   
  def randomLazyList[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.run(rng)))
   
  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

enum Result:
  case Passed
  case Falsified(
    failure: FailedCase, successes: SuccessCount)
 
  def isFalsified: Boolean = this match
    case Passed => false
    case Falsified(_, _) => true

object Gen:
  extension [A](self: Gen[A])
    def next(rng: RNG): (A, RNG) =
      self.run(rng) 

  def unit[A](a: => A): Gen[A] =
    State(rng => (a, rng))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
      boolean.flatMap(coin => 
          if(coin) g1 else g2)

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    State(rng =>
      val (i, rng2) = rng.nextInt
      val n = start + (math.abs(i)) % (stopExclusive - start)
      (n, rng2)
    )

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = 
    State(rng =>
      val sum = g1._2 + g2._2
      val (i, rng2) = RNG.double(rng)
      val c = i * sum 
      if(c < g1._2) 
        g1._1.run(rng2)
      else 
        g2._1.run(rng2)
    )
  def boolean: Gen[Boolean] = 
    State(rng =>
        val (i,rng2) = rng.nextInt
        val d1000 = math.abs(i) % 1000
        (d1000 >= 500, rng2))

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] =
      State(rng =>
        val (a, rng2) = self.run(rng)
        f(a).run(rng2)
      )
    def map[B](f: A => B): Gen[B] =
      flatMap(a => unit(f(a)))

  extension [A](self: Gen[A])
    def listOfN(n: Int): Gen[List[A]] = 
      def go(n: Int, acc: List[A], rng: RNG) : (List[A], RNG) =
        if(n > 0 ) then 
          val (i, rng2) = self.run(rng)
          go(n - 1, acc :+ i, rng2)
        else
          (acc, rng)
        
      State(rng => go(n, List.empty[A], rng))


trait SGen[+A]
