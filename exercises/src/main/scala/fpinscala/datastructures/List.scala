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
  // The answer has another implementation of init which is stack-safe.
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

  // Exercise 3.10
  // answer is different from the folding exercises
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    def go(acc: B, l: List[A]): B = l match {
      case Nil => acc
      case Cons(h, t) => go(f(acc, h), t)
    }
    go(z, l)
  }
  // Implemented in 1/11/2018
  // E3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _) // implement sum with foldLeft makes it tail-recursive.
  def product3(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
  def length3[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)
  // E3.12
  def reverse[A](l: List[A]) = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))
  // E3.13
  def foldRightByFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B) = foldLeft(reverse(as), z)((z, h) => f(h, z))
  def foldLeftByFoldRight[A, B](as: List[A], z: B)(f: (B, A) => B) = foldRight(reverse(as), z)((h, z) => f(z, h))
  // E3.14
  // def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)((a2, h) => Cons(h, a2))
  def appendViaFoldLeft[A](a1: List[A], a2: List[A]): List[A] = foldLeft(a1, a2)(Cons(_, _))
  // foldRight(l, Nil:List[A])(append) is the answer which is not stack-safe.
  // There is a comment about the solution. What does it mean?

  // E3.15
  def concat[A](xs: List[List[A]]): List[A] = foldRightByFoldLeft(xs, List[A]())((h, acc) => appendViaFoldLeft(h, acc))
  // E3.16
  def addOne(xs: List[Int]) = foldRight(xs, List[Int]())((h, acc) => Cons(h+1, acc))
  // E3.17
  def doubleToStringInList(xs: List[Double]): List[String] = foldRight(xs, Nil:List[String])((h, acc) => Cons(h.toString(), acc))
  // E3.18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, acc) => Cons(f(h), acc))
  // E3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil:List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc)
  // E3.20
  // def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((h, acc) => append(f(h), acc))
  // The answer looks more elegant.
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))
  // Implemented in 2/11/2018
  // E3.21
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(h => if (f(h)) List(h) else Nil)
  // At the end of the line, we return Nil as empty list. but when we pass empty list to fold as initial value, we must use nil:List[Int].
  // E3.22
  //  def addLists[Int](a: List[Int], b: List[Int]): List[Int] = (a, b) match {
  //    case (Nil, _) => Nil
  //    case (_, Nil) => Nil
  //    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addLists(t1, t2))
  //  }
  // Why declaring [Int] gives me problem? How can I solve the problem in a stack-safe way?
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addPairwise(t1,t2))
  }
  // E3.23
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }
  // E3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = ???

  // E3.29
}
