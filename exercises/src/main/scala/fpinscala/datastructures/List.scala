package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // Exercise 3.2
  // The function takes constant time
  def tail[A](l: List[A]): List[A] = l match {
//      case Nil => Nil
      case Nil => sys.error("tail of empty list")
      case Cons(_, xs) => xs
  }
  // Exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] = l match {
      case Nil => sys.error("head of empty list")
      case Cons(_, xs) => Cons(h, xs)
  }
  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
  {
//      case Nil => sys.error("empty list")
      if (n <= 0) l
      else {
        l match {
          case Nil => Nil
          case Cons(_, t) => drop(t, n - 1)
        }
      }
  }
  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
//    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile(t, f)// Pattern guard using condition before =>
    case _ => l
  }
  // Exercise 3.6
  // Why can’t this function be implemented in constant time like tail?
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }
  // Exercise 3.7 - Use foldRight to implement product with early termination which is impossible because foldRight
  // traverse the whole list until the end
  def productFoldRight(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(h, t) => foldRight(t, h)(_ * _)
  }
  // Exercise 3.8 ???
  // foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B
  // foldRight(List(1,2,3), Nil)(Cons(_,_))
  //foldRight(List(1,2,3), Nil:List[Nothing])(Cons(_,_))

  foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))

  // Exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  // Exercise 3.10 ???
  // answer is different from the folding exercises
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(acc: B, l: List[A]): B = l match {
      case Nil => acc
      case Cons(h, t) => go(f(acc, h), t)
    }
    go(z, l)
  }
  // Exercise 3.18


  def map[A,B](l: List[A])(f: A => B): List[B] = ???
  // Exercise 3.29
}
