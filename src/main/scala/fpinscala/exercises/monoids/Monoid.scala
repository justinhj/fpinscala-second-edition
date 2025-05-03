package fpinscala.exercises.monoids

import fpinscala.exercises.parallelism.Nonblocking.*
import fpinscala.exercises.testing.*

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
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = 
    val gen2 = gen ** gen ** gen
    val associativity = Prop.forAll(gen2) { case a ** b ** c =>
      m.combine(m.combine(a, b), c) == m.combine(a, m.combine(b, c))
    }
    val identity = Prop.forAll(gen) { a =>
      m.combine(m.empty, a) == a && m.combine(a, m.empty) == a
    }
    associativity && identity

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
    as.foldLeft(m.empty)((acc, a) => m.combine(acc,f(a)))

  def par[A](m: Monoid[A]): Monoid[Par[A]] = new:
    def empty = Par.unit(m.empty)
    def combine(a1: Par[A], a2: Par[A]): Par[A] =
      a1.map2(a2)(m.combine)

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = 
    val b = v.foldLeft(m.empty)((b,a) => m.combine(b,f(a)))
    // TODO this is cheating. The way to do it is (from the book)
    //   Implement a parallel version of foldMap using the library we developed in chapter 7. 
    //   Hint: Implement par, a combinator to promote Monoid[A] to a Monoid [Par[A]],5 and then
    // use this to implement parFoldMap:
    Par.unit(b)

  def orderedMonoid: Monoid[(Option[Int], Option[Int], Boolean)] = new:
    def empty: (Option[Int], Option[Int], Boolean) = (None, None, true)
    def combine(a1: (Option[Int], Option[Int], Boolean), a2: (Option[Int], Option[Int], Boolean)): (Option[Int], Option[Int], Boolean) = 
      val (lmin, lmax, lordered) = a1
      val (rmin, rmax, rordered) = a2
      val newMin = lmin.orElse(rmin)
      val newMax = rmax.orElse(lmax)
      val isOrdered = (lmax, rmin) match
        case (Some(l), Some(r)) => l <= r && lordered && rordered
        case _ => lordered && rordered
      (newMin, newMax, isOrdered)

  // Use a clever Monoid and foldmap 
  def ordered(ints: IndexedSeq[Int]): Boolean =
    val r = foldMap(ints.toList, orderedMonoid)(a => (Some(a),Some(a),true))
    r._3

  enum WC:
    case Stub(chars: String)
    case Part(lStub: String, words: Int, rStub: String)

  lazy val wcMonoid: Monoid[WC] = new:
    def toPartMaybe(wc: WC): WC = 
      wc match
        case p: WC.Part => p // parts remain parts
        case WC.Stub(chars) =>
          val splitted = chars.split("\\s+", -1)
          if(splitted.forall(_ == ""))
            wc // all empty can remain a stub
          else if(splitted.length == 1) // single words remain stubs
            wc
          else if(splitted.length == 2)
            WC.Part(splitted(0), 0, splitted.last)
          else if(splitted(0) == "" && splitted.last == "")
            WC.Part("", splitted.length - 2, "")
          else if(splitted.last == "")
            WC.Part(splitted(0), splitted.length - 2, "")
          else if(splitted(0) == "")
            WC.Part("", splitted.length - 2, splitted.last)
          else
            WC.Part(splitted(0), splitted.length - 2, splitted.last)

    def empty = WC.Stub("")
    def combine(a1: WC, a2: WC): WC = 
      val left = toPartMaybe(a1)
      val right = toPartMaybe(a2)
      (left,right) match
        case (WC.Part(ls1, lc, ls2), (WC.Part(rs1, rc, rs2))) => 
          var newCount = lc
          if ls2.length() > 0 || rs1.length() > 0 then
            newCount += 1
          newCount = newCount + rc
          WC.Part(ls1, newCount, rs2) 
        case (WC.Part(ls1, lc, ls2), (WC.Stub(rs1))) => 
          WC.Part(ls1, lc, ls2 + rs1)
        case (WC.Stub(ls1), WC.Part(rs1, rc, rs2)) => 
          WC.Part(ls1 + rs1, rc, rs2)
        case (WC.Stub(ls1), (WC.Stub(rs1))) => 
          WC.Stub(ls1 + rs1)

  def count(s: String): Int = 
    val c1 = wcMonoid.combine(WC.Part("",0,""), WC.Stub(s))
    val c2 = wcMonoid.combine(c1, WC.Part("",0,""))
    c2 match
      case WC.Stub(chars) => if(chars.forall(_ == ' ')) 0 else 1 
      case WC.Part(lStub, words, rStub) => words + (if lStub.length() > 0 then 1 else 0) + (if rStub.length() > 0 then 1 else 0)

  given productMonoid[A, B](using ma: Monoid[A], mb: Monoid[B]): Monoid[(A, B)] with
    def combine(x: (A, B), y: (A, B)) = 
      (ma.combine(x._1, y._1), mb.combine(x._2, y._2))
    val empty = (ma.empty, mb.empty)

  given functionMonoid[A, B](using mb: Monoid[B]): Monoid[A => B] with
    def combine(f: A => B, g: A => B) = 
      a => mb.combine(f(a),g(a))
    val empty: A => B = a => mb.empty

  given mapMergeMonoid[K, V](using mv: Monoid[V]): Monoid[Map[K, V]] with
    def combine(a: Map[K, V], b: Map[K, V]) = 
      b.foldLeft(a) { case (acc, (kb, vb)) =>
        acc.updated(kb, acc.get(kb).map(va => mv.combine(va, vb)).getOrElse(vb))
      }
    val empty = Map.empty

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    as.foldLeft(Map.empty[A, Int])((z, a) => 
        Monoid.mapMergeMonoid(using intAddition).combine(z, Map((a, 1)))
      )

end Monoid
