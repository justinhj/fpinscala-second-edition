package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  lazy val intAddition: Monoid[Int] = new:
    def empty = 0
    def combine(a1: Int, a2: Int): Int = a1 + a2

  lazy val intMultiplication: Monoid[Int] = new:
    def combine(a1: Int, a2: Int): Int = a1 * a2
    def empty = 1

  lazy val booleanOr: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    def empty = false

  lazy val booleanAnd: Monoid[Boolean] = new:
    def combine(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    def empty = true

  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(a1: Option[A], a2: Option[A]): Option[A] =
      (a1, a2) match
        case (None, None) => None
        case (Some(a), _) => Some(a)
        case (None, Some(a)) => Some(a)

    def empty = None

  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  def endoMonoid[A]: Monoid[A => A] = new:
    val empty = identity
    def combine(a1: A => A, a2: A => A): A => A =
      a => a2(a1(a))

  import fpinscala.exercises.testing.{Prop, Gen}
  // import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = ???

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)((b, a) => m.combine(b, a))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    var bacc = acc
    for(a <- as) {
      bacc = f(a, bacc)
    }
    bacc

  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    var bacc = acc
    for(a <- as) {
      bacc = f(bacc,a)
    }
    bacc

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    ???

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def empty = ???
    def combine(a1: Par[A], a2: Par[A]): Par[A] = ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    ???

  def ordered(ints: IndexedSeq[Int]): Boolean =
    ???

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = ???
    val empty = ???

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = ???
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = ???
    val empty = Map.empty

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???

end Monoid
