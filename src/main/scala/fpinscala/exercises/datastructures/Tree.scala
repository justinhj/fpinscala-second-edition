package fpinscala.exercises.datastructures

import java.rmi.UnexpectedException

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

  def size: Int = this match
    case Leaf(_) => 1
    case Branch(l, r) => 1 + l.size + r.size

  def depth: Int = 
    this match
      case Leaf(_) => 1 
      case Branch(l, r) => 1 +
        l.depth + r.depth

  def map[B](f: A => B): Tree[B] = 
    this match
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => 
        Branch(l.map(f), r.map(f))

  def fold[B](f: A => B, g: (B,B) => B): B = 
    this match 
      case Leaf(a) => f(a)
      case Branch(l, r) => 
        val lf = l.fold(f,g)
        val rf = r.fold(f,g)
        g(lf, rf)
  
  def sizeViaFold: Int =
    this.fold(_ => 1, (a,b) => a + b)
  
  def depthViaFold: Int =
    this.fold(_ => 1, (a,b) => math.max(a,b))
  
  def mapViaFold[B](f: A => B): Tree[B] =
    ???

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int =
    def go(t1: Tree[Int], fp: Option[Int]): Option[Int] = 
      t1 match
        case Leaf(v) if v > 0 => 
          fp match 
            case None => Some(v)
            case Some(a) => Some(a)
        case Leaf(_) => fp
        case Branch(l,r) =>
          val lo = go(l, fp)
          val ro = go(r, fp)
          (lo, ro) match
            case (Some(a), Some(b)) => throw new Exception()
            case (Some(a), _) => Some(a)
            case (_, Some(a)) => Some(a)
            case (None, None) => None
    go(t, None).get

  extension (t: Tree[Int]) def maximum: Int =
    def go(t1: Tree[Int], max: Int): Int = 
      t1 match
        case Leaf(v) => if(v > max) v else max
        case Branch(l,r) =>
          math.max(
            go(l, max),
            go(r, max))
    go(t, Integer.MIN_VALUE)

  extension (t: Tree[Int]) def maximumViaFold: Int =
    t.fold(identity, (a,b) => math.max(a,b))
