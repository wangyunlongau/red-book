package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // E3.25
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  // E3.26
  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(l, r) => maximum(l) max maximum(r)
  }
  // E3.27
  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0 // branch -> branch -> lead has path length of 2.
    case Branch(l, r) => depth(l) max depth(r) + 1
  }
  // E3.28
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  // E3.29 ???
  // cheat with taking a peek at the function signature of the answer
  // def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match
  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B) = t match {
    case Leaf(a) => f(a)
    case Branch(l, r) => g()
  }
}