package laziness

import Stream._

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

  def existsF(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)


  //def find(p: A => Boolean): Option[A] =
  //  filter(p).headOption

  // Exercise 5.1: Write a function to convert a Stream to a List, which
  // will force its evaluation and let you look at it in the REPL.
  def toList: List[A] = {
    def go(st: Stream[A], acc: List[A]): List[A] =
      st match {
        case Empty => acc.reverse
        case Cons(h, t) => go(t(), h() :: acc)
    }
    go(this, List())
  }

  // Exercise 5.2: Write the function take(n) for returning the first n
  // elements of a Stream, and drop(n) for skipping the first n elements.
  //
  // So cool!
  def take(n: Int): Stream[A] =
    if (n <= 0) Empty
    else this match {
      case Empty => Empty
      case Cons(h, t) => Cons(h, () => t().take(n-1))
    }

  def drop(n: Int): Stream[A] =
    if (n <= 0) this
    else this match {
      case Empty => Empty
      case Cons(h,t) => t().drop(n-1)
    }

  // Exercise 5.3: Write the function takeWhile for returning all starting elements
  // of a Stream that match the given predicate.
  //
  // Are there any issues with h being realized once and then rethunked?
  def takeWhile(p: A => Boolean): Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h,t) => {
        val head = h() // Is this a good work-around?
        if (p(head)) Cons(() => head, () => t().takeWhile(p))
        else Empty
      }
    }

  // Exercise 5.4: Implement forAll, which checks that all elements in the Stream
  // match a given predicate. Your implementation should terminate the traversal
  // as soon as it encounters a non-matching value.
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5: Use foldRight to implement takeWhile
  //
  // This seems like a really bad idea. Won't this hang on infinite streams?
  def takeWhileF(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => // Crappy Scala type inference is annoying
      if (p(h)) cons(h, t) else Empty)
      // No caching of lazy values, as they are passed to the lambda unwrapped
      // Or is cons vs. Cons sufficient?

  // Exercise 5.6: Hard: Implement headOption using foldRight
  def headOptionF: Option[A] =
    foldRight(Option.empty[A])((h,t) => Some(h))
  // Hm. The only hard thing here was dealing with Scala's weak type inference.
  // The last exercise seemed way harder.

  // Exercise 5.7: Implement map, filter, append, and flatMap using foldRight.
  // The append method should be non-strict in its argument.
  def map[B](f: A=>B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A=> Boolean): Stream[A] =
    foldRight(empty[A])((h,t) => if (p(h)) cons(h, t.filter(p)) else t.filter(p))

  def append[AA >: A](suffix: => Stream[AA]): Stream[A] =
    foldRight[Stream[A]](suffix.asInstanceOf[Stream[A]])((h, t) => cons(h, t))
  // This is completely horrible! There has to be a better way to do this.

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    map(f).foldRight(empty[B])((h,t) => h.append(t))
  // The types were the tricky thing. (Tried empty[Stream[B] for way too long.)

  // Exercise 5.13: Use unfold to implement map, take, takeWhile, zipWith, and
  // zipAll. The zipAll function should continue the traversal as long as either
  // stream has more elements--it uses Option to indicate whether each stream has
  // been exhausted
  def mapU[B](f: A => B): Stream[B] =
    unfold(this){
      case Empty => None
      case Cons(h,t) => Some((f(h()), t()))
    }

  def takeU(n: Int): Stream[A] =
    unfold((this,n)){
      case (as, i) =>
        if (i <= 0) None
        else as match {
          case Empty => None
          case Cons(h,t) => Some(h(), (t(), i - 1))
        }}

  def takeWhileU(p: A => Boolean): Stream[A] =
    unfold(this){
      case Empty => None
      case Cons(h,t) => {
        val head = h()
        if (p(head)) Some(head, t()) else None
      }}

  def zipWith[B,C](bs: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this,bs)){
      case (Cons(ah,at), Cons(bh, bt)) => Some((f(ah(), bh()), (at(), bt())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)){
      case (Empty, Empty) => None
      case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (Empty, bt()))
      case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), Empty))
      case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
    }
  // /Almost/ first try on this one...missed out a parenthesis

  // Exercise 5.14: Hard. Implement startsWith using functions you've written. It
  // should check if one Stream is a prefix of another. For instance,
  // Stream(1,2,3) startsWith Stream(1,2) would be true.
  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s).map {
      case (None, Some(_)) => false
      case (_, None) => true
      case (Some(a1), Some(a2)) => a1 == a2
    }.foldRight(true)(_ & _)

  // Exercise 5.15 Implement tails using unfold. For a given Stream, tails
  // returns the Stream of suffixes of the input sequence, starting with the
  // original Stream. For example, given Stream(1,2,3), it would return
  // Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
  //
  // This exercise gave me fits.
  def tails: Stream[Stream[A]] =
    unfold((this,false))(state => state match {
      case (_, true) => None // There has to be a better way to do this
      case (as, false) => as match {
        case Empty => Some(Empty, (Empty, true))
        case Cons(_, t) => Some((as, (t(), false)))
      }})

  def tails2: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]])(state => state match {
      case None => None
      case Some(as) => as match { // Have to nest to bind the as to the whole stream
        case Empty => Some(empty[A], None)
        case Cons(_, t) => Some((as, Some(t())))
      }})

  def tails3: Stream[Stream[A]] =
    unfold(Some(this): Option[Stream[A]])(state =>
      state.flatMap(s => s match {
        case Empty => Some(empty[A], None)
        case Cons(_, t) => Some(s, Some(t()))
      }))
  // This version actually ran the first try!


  // Exercise 5.16: Hard. Generalize tails to the function scanRight, which is
  // like a foldRight that returns a stream of the intermediate results. Your
  // function should reuse intermediate results so that traversing a Stream with
  // n elements always takes time linear in n. Can the function be implemented
  // using unfold? How, or why not? Could it be implemented using another function
  // we've written?


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

  val ones: Stream[Int] = Stream.cons(1, ones)

  // Exercise 5.8: Generalize ones slightly to the function constant, which returns
  // an infinite STream of a given value
  def constant [A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  // Exercise 5.9: Write a function that generates an infinite stream of integers,
  // starting from n, then n + 1, n + 2, and so on.
  def from (n: Int): Stream[Int] =
    if (n <= 0) Empty
    else Stream.cons(n, from(n + 1))

  // Exercise 5.10: Write a function fibs that generates the infinite stream of
  // Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.
  def fibs: Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      val c: Int = a + b
      cons(c, go(b, c))
    }
    cons(0, cons(1, go(0, 1)))
  }

  // Exercise 5.11: Write a more general stream-building function called unfold.
  // It takes an initial state, and a function for producing both the next state
  // and the next value in the generated stream.
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a,ss)) => cons(a, unfold(ss)(f))
    }
  // The type definition is interesting, and resulting in an implementation with
  // fewer special cases than my original approach.

  // Exercise 5.12: Write fibs, from, constant, and ones in terms of unfold.
  def fibsU: Stream[Int] =
    unfold((0,1))(
      (s => Some(s match {
        case (a,b) => (a, (b, a + b))
      })))
  // Fun!

  def fromU(n: Int): Stream[Int] =
    unfold(n)(i => Some((i, i + 1)))

  def constantU[A](a: A): Stream[A] =
    unfold(a)(aa => Some((aa, aa)))

  def onesU: Stream[Int] =
    unfold(1)(a => Some((1, 1)))

  // A couple of examples that put the Option to work
  def range(start: Int, end: Int): Stream[Int] =
    unfold(start)(i => if (i < end) Some(i, (i + 1)) else None)

  def skipRange(start: Int, end: Int, skip: Int): Stream[Int] =
    unfold(start)(i => if (i < end) Some(i, (i + skip)) else None)
}

object testCases {

}
