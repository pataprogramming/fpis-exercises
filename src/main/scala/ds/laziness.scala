package laziness

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h,t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def existsViaFold(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Exercise 5.1
  def toList: List[A] = {
    def go(st: Stream[A], acc: List[A]): List[A] =
      st match {
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List())
  }

  // Exercise 5.2 // So cool!
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))
    }

  // Exercise 5.3
  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(h,t) => t().drop(n-1)
    }

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  //def takeWhile(p: A => Boolean): Stream[A] =


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, t1: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty

  // Just converts any number of As in the arg list to equivalent memoizing stream
  // Seems backwards compared to Lisp's apply
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
}

object testCases {

}
