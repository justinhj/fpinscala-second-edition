package fpinscala.exercises.testing

import fpinscala.exercises.state.*
import fpinscala.exercises.parallelism.*
import fpinscala.exercises.parallelism.Par.Par
import Gen.*
import Prop.*
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

// opaque type Gen[+A] = State[RNG, A]

trait Prop

object Prop:
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???

object Gen:
  extension [A](self: Gen[A])
    // We should use a different method name to avoid looping (not 'run')
    def next(rng: RNG): (A, RNG) = ??? // self.run(rng)

  def unit[A](a: => A): Gen[A] = ???

  extension [A](self: Gen[A])
    def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait Gen[A]:
  def map[B](f: A => B): Gen[B] = ???
  def flatMap[B](f: A => Gen[B]): Gen[B] = ???

trait SGen[+A]
