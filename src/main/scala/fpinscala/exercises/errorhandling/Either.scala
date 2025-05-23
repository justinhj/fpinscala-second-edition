package fpinscala.exercises.errorhandling

// Hide std library `Either` since we are writing our own in this chapter
import scala.{Either as _, Left as _, Right as _}
import scala.util.control.NonFatal

enum Either[+E,+A]:
  case Left(get: E)
  case Right(get: A)

  def isLeft: Boolean =
    this match
      case Left(_) => true
      case _ => false

  def map[B](f: A => B): Either[E, B] = 
    this match
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match
      case Left(e) => Left(e)
      case Right(a) => 
        f(a)

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match
      case Left(e) => b
      case Right(a) => Right(a)

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this match
      case Left(e) => Left(e)
      case Right(a) =>
        b match 
          case Left(e) => Left(e)
          case Right(b) => 
            Right(f(a,b))

object Either:
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    val result = es.foldLeft(Right(List.empty[B]): Either[E, List[B]])((b,a) =>
        b match
          case Left(e) => Left(e)
          case Right(l) => 
            val res = f(a)
            res match
              case Left(e) => Left(e)
              case Right(a) => Right(a +: l))
    result.map(_.reverse)

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    traverse(es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if xs.isEmpty then
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = 
    try Right(x / y)
    catch case NonFatal(t) => Left(t)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B], f: (A, B) => C): Either[List[E], C] =
    (a,b) match
      case (Left(e1), Left(e2)) => Left(e1 ++ e2)
      case (Right(a), Right(b)) => Right(f(a,b))
      case (Left(e1), Right(_)) => Left(e1)
      case (Right(_), Left(e1)) => Left(e1)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldLeft(Right(List.empty[B]) : Either[List[E], List[B]])((acc, a) => 
        f(a) match
          case Left(err1) => 
            acc match
              case Left(err2) => Left(err1 ++ err2)
              case Right(_) => Left(err1)
          case Right(b) =>
            acc match
              case Left(err) => Left(err)
              case Right(bs) => Right(b +: bs)).map(_.reverse)

  def sequenceAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)
