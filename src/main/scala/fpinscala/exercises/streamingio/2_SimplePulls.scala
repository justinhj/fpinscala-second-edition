package fpinscala.exercises.streamingio

import fpinscala.answers.iomonad.{IO, Monad}
import fpinscala.answers.monoids.Monoid

object SimplePulls:

  enum Pull[+O, +R]:
    // Three types of Pull, the Result that delivers a final value of R
    // Output which is an intermediate output of type O 
    // and FlatMap which composes two Pulls. (?)
    case Result[+R](result: R) extends Pull[Nothing, R]
    case Output[+O](value: O) extends Pull[O, Unit]
    case FlatMap[X, +O, +R](source: Pull[O, X], f: X => Pull[O, R]) extends Pull[O, R]

    // Advance the stream. Returning either a result in the Left
    // or a tuple of the output and the rest of the stream in the Right.
    def step: Either[R, (O, Pull[O, R])] = this match
      case Result(r) => Left(r)
      case Output(o) => Right(o, Pull.done)
      case FlatMap(source, f) => 
        source match
          // Here we handle that the left hand side is also a flatmap, 
          // by recursively calling step on the left hand side.
          case FlatMap(s2, g) => s2.flatMap(x => g(x).flatMap(y => f(y))).step
          case other => other.step match
            // If the left hand side is a result, we apply the function f to it.
            case Left(r) => f(r).step
            // And if the left hand side is an output, we return the output and the rest of the stream.
            case Right((hd, tl)) => Right((hd, tl.flatMap(f)))

    @annotation.tailrec
    final def fold[A](init: A)(f: (A, O) => A): (R, A) = 
      step match
        case Left(r) => (r, init)
        case Right((hd, tl)) => tl.fold(f(init, hd))(f)

    def toList: List[O] =
      fold(List.newBuilder[O])((bldr, o) => bldr += o)(1).result

    def flatMap[O2 >: O, R2](f: R => Pull[O2, R2]): Pull[O2, R2] =
      FlatMap(this, f)

    def >>[O2 >: O, R2](next: => Pull[O2, R2]): Pull[O2, R2] =
      flatMap(_ => next)

    def map[R2](f: R => R2): Pull[O, R2] =
      flatMap(r => Result(f(r)))

    def repeat: Pull[O, Nothing] =
      this >> repeat

    def uncons: Pull[Nothing, Either[R, (O, Pull[O, R])]] =
      Pull.done >> Result(step)

    def take(n: Int): Pull[O, Option[R]] =
      if n <= 0 then Result(None)
      else uncons.flatMap:
        case Left(r) => Result(Some(r))
        case Right((hd, tl)) => Output(hd) >> tl.take(n - 1)

    // Exercise 15.3
    def drop(n: Int): Pull[O, R] =
      if (n > 0) then
        uncons.flatMap:
          case Left(r) => Result(r)
          case Right((_, tl)) => tl.drop(n - 1)
      else this

    // Exercise 15.3
    def takeWhile(f: O => Boolean): Pull[O, Pull[O, R]] =
      uncons.flatMap:
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if f(hd) then Output(hd) >> tl.takeWhile(f)
          else Result(tl)
    
    // Exercise 15.3
    def dropWhile(f: O => Boolean): Pull[Nothing, Pull[O, R]] =
      uncons.flatMap:
        case Left(r) => Result(Result(r))
        case Right((hd, tl)) =>
          if f(hd) then tl.dropWhile(f)
          else Result(Output(hd) >> tl)

    def mapOutput[O2](f: O => O2): Pull[O2, R] =
      uncons.flatMap:
        case Left(r) => Result(r)
        case Right((hd, tl)) => Output(f(hd)) >> tl.mapOutput(f)

    def filter(p: O => Boolean): Pull[O, R] =
      uncons.flatMap:
        case Left(r) => Result(r)
        case Right((hd, tl)) =>
          (if p(hd) then Output(hd) else Pull.done) >> tl.filter(p)

    def exists(p: O => Boolean): Pull[Boolean, Unit] =
      uncons.flatMap:
        case Left(r) => Output(true) >> Result(())
        case Right((hd, tl)) =>
          if p(hd) then (Output(true) >> Result(())) else (Pull.done >> tl.exists(p))

    def count: Pull[Int, R] =
      def go(total: Int, p: Pull[O, R]): Pull[Int, R] =
        p.uncons.flatMap:
          case Left(r) => Result(r)
          case Right((_, tl)) =>
            val newTotal = total + 1
            Output(newTotal) >> go(newTotal, tl)
      Output(0) >> go(0, this)

    // Exercise 15.4
    def tally[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
      def go(acc: O2, p: Pull[O, R]): Pull[O2, R] =
        p.uncons.flatMap:
          case Left(r) => Result(r)
          case Right((hd, tl)) =>
            val next = m.combine(hd, acc)
            Output(next) >> go(next, tl)
      Output(m.empty) >> go(m.empty, this)

    def mapAccumulate[S, O2](init: S)(f: (S, O) => (S, O2)): Pull[O2, (S, R)] =
      uncons.flatMap:
        case Left(r) => Result((init, r))
        case Right((hd, tl)) =>
          val (s, out) = f(init, hd)
          Output(out) >> tl.mapAccumulate(s)(f)

    // Exercise 15.6
    def countViaMapAccumulate: Pull[Int, R] =
      mapAccumulate(0)((s, _) => (s + 1, s)).map(_._2)

    // Exercise 15.6
    def tallyViaMapAccumulate[O2 >: O](using m: Monoid[O2]): Pull[O2, R] =
      mapAccumulate(m.empty)((s, o) => (m.combine(s, o), m.combine(s, o))).map(_._2)

  object Pull:
    val done: Pull[Nothing, Unit] = Result(())

    def fromList[O](os: List[O]): Pull[O, Unit] =
      os match
        case Nil => done
        case hd :: tl => Output(hd) >> fromList(tl)

    def fromLazyList[O](os: LazyList[O]): Pull[O, Unit] =
      os match
        case LazyList() => done
        case hd #:: tl => Output(hd) >> fromLazyList(tl)

    def unfold[O, R](init: R)(f: R => Either[R, (O, R)]): Pull[O, R] =
      f(init) match
        case Left(r) => Result(r)
        case Right((o, r2)) => Output(o) >> unfold(r2)(f)

    // Exercise 15.1
    def fromListViaUnfold[O](os: List[O]): Pull[O, Unit] =
      unfold(os)(os =>
        os match
          case Nil => Left(Nil)
          case hd :: tl => Right((hd, tl))
          ).map(_ => ())

    // Exercise 15.1
    def fromLazyListViaUnfold[O](os: LazyList[O]): Pull[O, Unit] =
      unfold(os)(os =>
        os match
          case hd #:: tl => Right((hd, tl))
          case ll => Left(LazyList.empty)
          ).map(_ => ())

    def continually[O](o: O): Pull[O, Nothing] =
      Output(o) >> continually(o)

    def continuallyViaRepeat[O](o: O): Pull[O, Nothing] =
      Output(o).repeat

    // Exercise 15.2
    def iterate[O](initial: O)(f: O => O): Pull[O, Nothing] =
      Output(initial) >> iterate(f(initial))(f) 

    extension [R](self: Pull[Int, R])
      // Exercise 15.5
      def slidingMean(n: Int): Pull[Double, R] =
        def avg(l: List[Int]): Double =
          l.sum.toDouble / l.length
        def go(lastN: List[Int], p: Pull[Int, R]): Pull[Double, R] =
          p.uncons.flatMap:
            case Left(r) => Result(r)
            case Right((hd, tl)) =>
              val newN = (lastN :+ hd).takeRight(n)
              Output(avg(newN)) >> go(newN, tl)
        val lastN = List.fill(n)(0)
        Output(0.0) >> go(lastN, self)

      // Exercise 15.6
      def slidingMeanViaMapAccumulate(n: Int): Pull[Double, R] =
        def f(s: List[Int], o: Int): (List[Int], Double) =
          val newS = (s :+ o).takeRight(n)
          (newS, newS.sum.toDouble / newS.length)
        self.mapAccumulate(List.fill(n)(0))(f).map(_._2)

    given [O] => Monad[[x] =>> Pull[O, x]]:
      def unit[A](a: => A): Pull[O, A] = Result(a)
      extension [A](pa: Pull[O, A])
        def flatMap[B](f: A => Pull[O, B]): Pull[O, B] =
          pa.flatMap(f)

    extension [O](self: Pull[O, Unit])
      def flatMapOutput[O2](f: O => Pull[O2, Unit]): Pull[O2, Unit] =
        self.uncons.flatMap:
          case Left(()) => Result(())
          case Right((hd, tl)) =>
            f(hd) >> tl.flatMapOutput(f)
    val outputMonad: Monad[[x] =>> Pull[x, Unit]] = new:
      def unit[A](a: => A): Pull[A, Unit] = Output(a)
      extension [A](pa: Pull[A, Unit])
        def flatMap[B](f: A => Pull[B, Unit]): Pull[B, Unit] =
          pa.flatMapOutput(f)

    extension [O](self: Pull[O, Unit])
      def toStream: Stream[O] = self
  
  opaque type Stream[+O] = Pull[O, Unit]
  object Stream:
    def apply[O](os: O*): Stream[O] =
      Pull.fromList(os.toList).toStream
    extension [O](self: Stream[O])
      def toPull: Pull[O, Unit] = self

      def fold[A](init: A)(f: (A, O) => A): A = 
        self.fold(init)(f)(1)

      def toList: List[O] =
        self.toList

      def take(n: Int): Stream[O] =
        self.take(n).void

      def filter(p: O => Boolean): Stream[O] =
        self.filter(p).toStream

      def ++(that: => Stream[O]): Stream[O] =
        self >> that

    given Monad[Stream]:
      def unit[A](a: => A): Stream[A] = Pull.Output(a)
      extension [A](sa: Stream[A])
        def flatMap[B](f: A => Stream[B]): Stream[B] =
          sa.flatMapOutput(f)

  type Pipe[-I, +O] = Stream[I] => Stream[O]

end SimplePulls

object SimplePullExamples:
  import SimplePulls.{Pipe, Stream, Pull}

  val nonEmpty: Pipe[String, String] =
    _.filter(_.nonEmpty)

  val lowerCase: Pipe[String, String] =
    _.map(_.toLowerCase)

  val normalize: Pipe[String, String] =
    nonEmpty andThen lowerCase

  val lines = Stream("Hello", "", "World!")
  val normalized = normalize(lines)
  import scala.util.chaining.scalaUtilChainingOps
  val normalized2 = lines.pipe(normalize)
  val normalized3 = lines.pipe(nonEmpty).pipe(lowerCase)

  def count[A]: Pipe[A, Int] = _.toPull.count.void.toStream

  // Exercise 15.7
  def exists[I](f: I => Boolean): Pipe[I, Boolean] =
    _.toPull.mapOutput(a => f(a)).tally(using Monoid.booleanOr).toStream

  // Exercise 15.7
  // We want it to consume the stream and return false if it does not find 
  // an element or true and halt when it does
  def existsHalting[I](f: I => Boolean): Pipe[I, Boolean] =
    _.toPull.exists(f).toStream

  def countGt40K[I]: Pipe[I, Boolean] =
    count andThen existsHalting(_ > 40000)

  def fromIterator[O](itr: Iterator[O]): Stream[O] =
    Pull.unfold(itr)(itr =>
      if itr.hasNext then Right((itr.next(), itr))
      else Left(itr)
    ).void.toStream

  def processFile[A](
    file: java.io.File,
    p: Pipe[String, A],
  )(using m: Monoid[A]): IO[A] = IO:
    val source = scala.io.Source.fromFile(file)
    try fromIterator(source.getLines).pipe(p).fold(m.empty)(m.combine)
    finally source.close()

  def pipeFile[A](
    in: java.io.File,
    out: java.io.File,
    p: Pipe[String, String],
  ): IO[Unit] = IO:
    val source = scala.io.Source.fromFile(in)
    try 
      fromIterator(source.getLines).pipe(p).fold(List.newBuilder[String])((bldr, o) => bldr += o).result.foreach { line =>
        val writer = new java.io.PrintWriter(new java.io.FileWriter(out, true))
        try writer.println(line)
        finally writer.flush()
      }

  def checkFileForGt40(file: java.io.File): IO[Boolean] =
    processFile(file, count andThen exists(_ > 40))(using Monoid.booleanOr)

  def toCelsius(fahrenheit: Double): Double =
    (5.0 / 9.0) * (fahrenheit - 32.0)

  def converterWrapper(in: String) : String = 
    val fahrenheit = in.toDouble
    val celsius = toCelsius(fahrenheit)
    celsius.toString

  def convert(inputFile: String, outputFile: String): IO[Unit] =
    val in = new java.io.File(inputFile)
    val out = new java.io.File(outputFile)
    pipeFile(in, out, _.map(converterWrapper))
