package err

import scala.math

trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A=>B): Option[B] =
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }
  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(v) => f(v)
    }
  def getOrElse[B >: A](default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def filter(f: A=> Boolean): Option[A] =
    this match {
      case None => None
      case Some(v) => if (f(v)) Some(v) else None
    }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap((m: Double) => (mean(xs.map( (x: Double) => math.pow(x - m, 2)))))
  }

  // Exercise 4.3: Write a generic function map2 that combines two Option
  // values using a binary funciton. If either Option value is None, then
  // the return value is too.
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] =
    (a,b) match {
      case (Some(aval),Some(bval)) => Some(f(aval, bval))
      case _ => None
    }

  // Exercise 4.4: Write a function sequence that combines a list of Options into
  // one Option containing a list of all the Some values in the original list. If
  // the original list contains None even once, the result of the function should
  // be None; otherwise the result should be Some with a list of all the values.
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], acc: List[A]): Option[List[A]] =
      a match {
        case None :: t => None
        case Some(v) :: t => go(t, v :: acc)
        case _ => Some(acc.reverse)
      }
    go(a, Nil)
  }

  // Exercise 4.5: Implement the traverse function
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(as: List[A], acc: List[B]): Option[List[B]] =
      as match {
        case Nil => Some(acc.reverse)
        case a :: as => f(a) match {
          case None => None
          case Some(v) => go(as, v :: acc)
        }
      }
    go(as, Nil: List[B])
  }

}

trait Either[+E, +A] {
  // Exercise 4.6: Implement versions of map, flatMap, orElse, and map2 on
  // Either that operate on the Right value.
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(a) => try Right(f(a)) catch { case e: E => Left(e) } // ???
      case Left(e) => Left(e) // ??? Cast Right to Nothing
    }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => try Right(f(a))
      case Left(e) => Left(e)
    }
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b // ??? Double-check
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(aa), Right(bb)) => try Right(f(aa,bb)) catch { case e: E => Left(e) }
      case Left(ee) => Left(ee)
    }

}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
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

object testCases {
  val a = Some(2.0)
  val b = Some("foo")
  val c = None
}
