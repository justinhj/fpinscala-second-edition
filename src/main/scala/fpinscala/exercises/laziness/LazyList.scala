package fpinscala.exercises.laziness

import fpinscala.answers.testing.Gen.maxProp

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def toList: List[A] =
    this match
      case Empty => Nil
      case Cons(h, t) => h() :: t().toList
    

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the lazy list. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)

  def take(n: Int): LazyList[A] = 
    if n == 0 then LazyList.empty
    else 
      this match
        case Empty => LazyList.empty
        case Cons(h, t) => LazyList.cons(h(), t().take(n - 1))

  def drop(n: Int): LazyList[A] =
    if n == 0 then this
    else 
      this match
        case Empty => LazyList.empty
        case Cons(h, t) => t().drop(n - 1)

  def takeWhile(p: A => Boolean): LazyList[A] =
    this match
      case Empty => LazyList.empty
      case Cons(h, t) => if p(h()) then LazyList.cons(h(), t().takeWhile(p)) else LazyList.empty

  def forAll(p: A => Boolean): Boolean =
    this match
      case Empty => true
      case Cons(h, t) => p(h()) && t().forAll(p)

  def headOption: Option[A] = 
    this match
      case Empty => None
      case Cons(h, _) => Some(h())

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: LazyList[B]): Boolean = 
    this match
      case Empty => s == LazyList.empty
      case Cons(h, t) => s match
        case Empty => true
        case Cons(h2, t2) => h() == h2() && t().startsWith(t2())

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] = 
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty 
    else cons(as.head, apply(as.tail*))

  val ones: LazyList[Int] = LazyList.cons(1, ones)

  def continually[A](a: A): LazyList[A] =
    LazyList.cons(a, continually(a))

  def from(n: Int): LazyList[Int] = 
    LazyList.cons(n, from(n + 1))

  lazy val fibs: LazyList[Int] =
    def go(a: Int, b: Int): LazyList[Int] = 
      LazyList.cons(a, go(b, a + b))
    go(0, 1)

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] =
    f(state) match
      case None => LazyList.empty
      case Some((h, s)) => LazyList.cons(h, unfold(s)(f))

  lazy val fibsViaUnfold: LazyList[Int] = 
    unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromViaUnfold(n: Int): LazyList[Int] = 
    unfold(n)(s => Some((s, s + 1)))

  def continuallyViaUnfold[A](a: A): LazyList[A] = 
    unfold(a)(s => Some((s, s)))

  lazy val onesViaUnfold: LazyList[Int] =
    unfold(1)(_ => Some(1,1))