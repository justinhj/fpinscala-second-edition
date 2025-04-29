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
      case Leaf(_) => 0 
      case Branch(l, r) => 1 +
        math.max(l.depth, r.depth)

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
    this.fold(_ => 1, (a,b) => 1 + a + b)
  
  def depthViaFold: Int =
    this.fold(_ => 0, (a,b) => 1 + math.max(a,b))
  
  // fold takes A => B and a combine function for B
  // since we want a Tree[B] then B is Tree of B
  // Fold accumulates a value B on the tree[A]
  // It applies f on the leaf only to make B's
  // at branches it combines branches using g
  // So f(a) should create a leaf
  // and g should combine branches
  def mapViaFold[B](f: A => B): Tree[B] =
    this.fold(a => Leaf(f(a)), (b1,b2) => Branch(b1, b2))

object Tree:

  def size[A](t: Tree[A]): Int = t match
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)

  extension (t: Tree[Int]) def firstPositive: Int =
    t match
      case Leaf(i) => i
      case Branch(l,r) =>
        val lb = l.firstPositive
        if(lb > 0) lb else r.firstPositive

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
