package fpinscala.exercises
package monads

import parsing.*
import testing.*
import parallelism.*
import state.*
import parallelism.Par.*

trait Functor[F[_]]:
  extension [A](fa: F[A])
    def map[B](f: A => B): F[B]

  extension [A, B](fab: F[(A, B)]) def distribute: (F[A], F[B]) =
    (fab.map(_(0)), fab.map(_(1)))

  extension [A, B](e: Either[F[A], F[B]]) def codistribute: F[Either[A, B]] =
    e match
      case Left(fa) => fa.map(Left(_))
      case Right(fb) => fb.map(Right(_))

object Functor:
  given listFunctor: Functor[List]:
    extension [A](as: List[A])
      def map[B](f: A => B): List[B] = as.map(f)

trait Monad[F[_]] extends Functor[F]:
  def unit[A](a: => A): F[A]

  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B] =
      fa.map(f).join
    
    def map[B](f: A => B): F[B] =
      fa.flatMap(a => unit(f(a)))

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] =
      fa.flatMap(a => fb.map(b => f(a, b)))

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldLeft(unit(Nil)) { (acc, fa) =>
      acc.flatMap(as => fa.map(a => a :: as))
    }

  def traverse[A, B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(Nil: List[B])) { (a, acc) =>
      f(a).map2(acc)(_ :: _)
    }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    val nFas = List.fill(n)(fa)
    sequence(nFas)

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => f(a).flatMap(b => g(b))

  extension [A](fa: F[A])
    def flatMapViaCompose[B](f: A => F[B]): F[B] =
      // Two functions are the parameters
      // the first is A => F[B]
      // the second is B => F[C]
      // we want a function that is A => F[C]
      // the input is fa and the output is fb
      // a: F[A] => F[B]
      compose((a: F[A]) => a.map(identity), f)(fa)

  // Basically sequence with an effectful if
  def filterM[A](as: List[A])(f: A => F[Boolean]): F[List[A]] =
    as.foldLeft(unit(Nil)) { (acc, a) =>
      acc.flatMap(as => f(a).map(ab => if(ab) a :: as else as))
    }

  extension [A](ffa: F[F[A]]) def join: F[A] =
    ffa.flatMap(identity)

  extension [A](fa: F[A])
    def flatMapViaJoinAndMap[B](f: A => F[B]): F[B] =
      fa.map(f).join

  def composeViaJoinAndMap[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => 
      f(a).map(g).join

end Monad      

object Monad:
  given genMonad: Monad[Gen]:
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    extension [A](fa: Gen[A])
      override def flatMap[B](f: A => Gen[B]): Gen[B] =
        Gen.flatMap(fa)(f)

  given parMonad: Monad[Par]:
    def unit[A](a: => A) = ???
    extension [A](fa: Par[A])
      override def flatMap[B](f: A => Par[B]): Par[B] =
        ???

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = new:
    def unit[A](a: => A) = ???
    extension [A](fa: P[A])
      override def flatMap[B](f: A => P[B]): P[B] =
        ???

  given optionMonad: Monad[Option]:
    def unit[A](a: => A) = Option(a)
    extension [A](fa: Option[A])
      override def flatMap[B](f: A => Option[B]) =
        fa match
          case Some(a) => f(a)
          case None => None
        

  given lazyListMonad: Monad[LazyList]:
    def unit[A](a: => A) = LazyList(a)
    extension [A](fa: LazyList[A])
      override def flatMap[B](f: A => LazyList[B]) =
        if(fa.isEmpty) LazyList.empty
        else
          val mapped = fa.map(f)
          mapped.flatten

  given listMonad: Monad[List]:
    def unit[A](a: => A) = List(a)
    extension [A](fa: List[A])
      override def flatMap[B](f: A => List[B]) =
        val mapped = fa.map(f)
        mapped.flatten

end Monad

case class Id[+A](value: A):
  def map[B](f: A => B): Id[B] =
    Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] =
    Id(f(value)).value

object Id:
  given idMonad: Monad[Id]:
    def unit[A](a: => A) = Id(a)
    extension [A](fa: Id[A])
      override def flatMap[B](f: A => Id[B]) =
        fa.flatMap(f)

opaque type Reader[-R, +A] = R => A

object Reader:
  extension [R, A](ra: Reader[R, A])
    def run(r: R): A = ra(r)

  given readerMonad[R]: Monad[Reader[R, _]] with
    def unit[A](a: => A): Reader[R, A] = 
      r => a
    extension [A](fa: Reader[R, A])
      override def flatMap[B](f: A => Reader[R, B]): Reader[R,B] =
         r => f(fa.run(r))(r)
