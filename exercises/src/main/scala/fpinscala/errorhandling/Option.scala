package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
//    this match {
//    case None => None
//    case Some(a) => f(a)
//    }

  def orElse[B>:A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob
  def orElsePatternMatch1[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }
  def orElsePatternMatch2[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)

  def filterPatternMatch1(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(a) => if (f(a)) Some(a) else None
  }

  def filterPatternMatch2(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _ => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
//    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    //    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh::tt))
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  // traverse vs sequence
  // unlike sequence, traverse takes a function to transform List[A] to List[Option[B]],
  // and then from List[Option[B]] to Option[List[B]]
  // both will return None if any element in List[Option[A]] is None.

  def sequenceFoldRight[A](a: List[Option[A]]): Option[List[A]] =
//    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => a.flatMap(aa => b.map(bb => aa::bb)))
    a.foldRight[Option[List[A]]](Some(Nil))((a, b) => map2(a, b)(_ :: _))

  def traverseFoldRight[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((a, b) => map2(f(a), b)(_ :: _))

  def sequenceByTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)
}
