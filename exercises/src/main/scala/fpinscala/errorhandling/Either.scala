package fpinscala.errorhandling

import scala.{
  Option => _,
  Either => _,
  Left => _,
  Right => _,
  _
} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(success) => Right(f(success))
    case Left(failure)  => Left(failure)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(success) => f(success)
    case Left(e)        => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(success) => Right(success)
    case Left(_)        => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for { a <- this; b1 <- b } yield f(a, b1)
  //   this.flatMap[Either[EE, C]](a => b.map(b1 => f(a, b1))) doesn't work which is weird. why do I have to use a for-comprehension to do that?!
}
case class Left[+E](get: E) extends Either[E, Nothing]
case class Right[+A](get: A) extends Either[Nothing, A]

object Either {
  def traverse[E, A, B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es match {
      case Nil => Right(Nil)
//    case h :: t => map2(f(h), traverse(t)(f))(_ :: _) why this one doesn't work?
      case h :: t => (f(h) map2 traverse(t)(f))(_ :: _)
    }

//    es match {
//      case Nil    => Right(Nil)
//      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt))
//    }

  def traverseFoldRight[E, A, B](es: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(Nil))(
      (a, b) => (f(a) map2 b)(_ :: _))

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}
