package ds

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Exercise 3.1:
//
// val x = List(1,2,3,4,5) match {
//   case Cons(x, Cons(2, Cons(4, _))) => x
//   case Nil => 42
//   case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
//   case Cons(h, t) => h + sum(t)
//   case _ => 101
// }
//
// Without execution, it can be seen that the first matching case will be
// the third, with 'x' bound to 1, 'y' bound to 2, the '_' matching
// the 'Cons(5,Nil)' at the end (since it doesn't care), and thus
// the whole expression will return x+y = 1+2 = 3.


object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil         => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0,_) => 0.0
    case Cons(x,xs)  => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3:2 Implement tail
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil        => Nil
      case Cons(_, t: List[A]) => t // Why is type necessary here?
    }

  // Exercise 3.3:
  def setHead[A](l: List[A], e: A): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t: List[A]) => Cons(e, t)
    }


  // Exercise 3.4:
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, t: List[A]) =>
        if (n <= 0) l
        else drop(t, n-1)
    }


  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A=> Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(h, t: List[A]) =>
        if (!f(h)) l
        else dropWhile(t, f)
    }


  // Exercise 3.6

  // Helper function to simplify the logic of init
  def reverse[A](l: List[A]): List[A] = {
    def go(ret: List[A], rem: List[A]): List[A] =
      rem match {
        case Nil => ret
        case Cons(h, t) => go(Cons(h, ret), t)
      }
    go(Nil, l)
  }

  // Chop of the last element using reverse; roughly as efficient
  // as implementing the logic directly...the list has to be rebuilt twice,
  // once to traverse it and find the end, the second to put it into the
  // correct order (as cons-style linked-lists are not doubly linked)
  def init[A](l: List[A]): List[A] =
    reverse(tail(reverse(l)))

  // def init[A](l: List[A]): List[A] = {
  //   def go(ret: List[A], rem: List[A]): List[A] =
  //     rem match {
  //       case Nil => ret
  //       case Cons(h, Nil) => ret
  //       case Cons(h, t: List[A]) => go(Cons(h, ret), t)
  //   }
  //   go(Nil, l)
  //}

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercsie 3.7: Nope, can't short circuit from the "inside". foldRight
  // recurses based only on the length of the list, so the provided function
  // is not able to affect the flow of execution this model.

  // Exercise 3.8: Not quite sure about the phrasing of this one

  // Exercise 3.9: Implement length using foldRight
  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, acc) => acc + 1)

  // Exercise 3.10: Implement foldLeft
  def foldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    def go(as: List[A], acc: B): B =
      as match {
        case Nil => acc
        case Cons(x, xs) => go(xs, f(x, acc))
      }
    go(as, z)
  }

  // Exercise 3:11 Wrtie sum, product, and lenth using foldLeft
  def sumFold(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

  def productFold(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

  def lengthFold[_](as: List[_]): Int =
    foldLeft(as, 0)((_,len) => 1 + len)

  // Exercise 3.12: Write reverse using fold (note that an incidental
  // reverse was constructed for Exercise 3.6
  def reverseFold[A](as: List[A]): List[A] =
    foldLeft(as, Nil:List[A])(Cons(_,_))

  // Exercise 3:13: Can foldLeft be written in terms of foldRight? Can
  // foldRight be written in terms of foldLeft?

  // Exercise 3.14: Implement append in terms of foldLeft or foldRight
  def appendFold[A](as: List[A], t: List[A]): List[A] =
    foldLeft(reverseFold(as),t)(Cons(_,_))

  // Exercise 3.15: Write a function that concatenates a list of lists
  // into a single list. Runtime should be linear in the length of all lists.

  def concat[A](ass: List[List[A]]): List[A] = {
    def recur(ass: List[List[A]], acc: List[A]): List[A] =
      ass match {
        case Nil => acc
        case Cons(as, t) =>
          recur(t, appendFold(as, acc))
      }
    recur(reverse(ass), Nil:List[A])
  }

  // Exercise 3:16: Write a function that transforms a list of integers by adding
  // 1 to each element
  def addOneToEach(is: List[Int]): List[Int] =
    foldLeft(reverse(is), Nil:List[Int])((a,b) => Cons(1 + a, b))

  // Exercose 3.17: Write a function that turns each value in a List[Double] into
  // a String.
  def stringifyDoubles(ds: List[Double]): List[String] =
    foldLeft(reverse(ds), Nil:List[String])((a,b) => Cons(a.toString, b))

  // Exercise 3.18: Write a function map that generalizes modifying each element
  // in a list while maintaining the structure of the list.

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(reverse(as), Nil: List[B])((a,b) => Cons(f(a), b))

  // Exercise 3.19: Write a function filter that removes elements from a list
  // unless they satisfy a given predicate. Use it to remove all odd numbers from
  // a List[Int]
  def filter[A](as: List[A])(p: A => Boolean): List[A] = {
    def recur(as: List[A], acc: List[A]): List[A] =
      as match {
        case Nil => acc
        case Cons(a, t) =>
          if (p(a)) recur(t, Cons(a, acc))
          else recur(t, acc)
      }
    recur(reverse(as), Nil:List[A])
  }

  def isEven(n: Int): Boolean =  (n % 2) == 0

  // Exercise 3.20: Write a function flatMap that works liek map
  // except that the function given will return a list instead of a
  // single result, and the list should be inserted into the final
  // resulting list.
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // Exercise 3.21: Use flatMap to implement filter.
  def flatFilter[A](as: List[A])(p: A => Boolean): List[A] =
    flatMap(as)((a) => if (p(a)) List(a) else Nil)

  // Exercise 3.22: Write a function that accepts two lists and constructs a
  // new list by adding corresponding elements.
  def addLists(l1: List[Int], l2: List[Int]): List[Int] = {
    def recur(l1: List[Int], l2: List[Int], acc: List[Int]): List[Int] =
      l1 match {
        case Nil => appendFold(l2, acc)
        case Cons(h1, t1) => l2 match {
          case Nil => appendFold(l1, acc)
          case Cons(h2, t2) => recur(t1, t2, Cons(h1 + h2, acc))
        }
      }
    reverse(recur(l1, l2, Nil: List[Int]))
  }

  // Exercise 3.23: Generalize the function from 3.22 so it's not specific
  // to integers or addition.
  def zipWith[A](l1: List[A], l2: List[A])(f: (A,A) => A): List[A] = {
    def recur(l1: List[A], l2: List[A], acc: List[A]): List[A] =
      l1 match {
        case Nil => appendFold(l2, acc)
        case Cons(h1, t1) => l2 match{
          case Nil => appendFold(l1, acc)
          case Cons(h2, t2) => recur(t1, t2, Cons(f(h1,h2), acc))
        }
      }
    reverse(recur(l1, l2, Nil: List[A]))
  }

  // Exercise 3.24: Implent hasSubsequence for checking whether a List
  // contains another List as a subsequence.
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def recur(sup: List[A], sub: List[A]): Boolean = {
      (sup, sub) match {
        case (_, Empty) => true
        case (Empty, Cons(_,_)) => false
        case (Cons(suph, supt), Cons(subh, subt)) =>
          (suph == supt) && hasSubsequence(suph, supt)
      }}
  }



}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size(t: Tree[_]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => size(l) + size(r) + 1
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(n) => n
      case Branch(l,r) => maximum(l) max maximum(r)
    }

  def depth(t: Tree[_]): Int = {
    def descend(t: Tree[_], d: Int): Int =
      t match {
        case Leaf(_) => d
        case Branch(l,r) => descend(l, d+1) max descend(r, d+1)
      }
    descend(t, 0)
  }

  def treeMap[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(treeMap(l)(f), treeMap(r)(f): Tree[B])
    }
  }

  def treeFold[A,B](t: Tree[A])(f: A=>B)(comb: (B,B)=> B): B = {
    def descend(t: Tree[A]): B =
      t match {
        case Leaf(v) => f(v)
        case Branch(l,r) => comb(descend(l), descend(r))
      }
    descend(t)
  }

}



object testCases {
  val t1: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)),Branch(Leaf(3),Leaf(4)))
  val t2: Tree[String] = Branch(Branch(Leaf("a"),Branch(Leaf("b"),Leaf("c"))),Branch(Leaf("d"),Leaf("e")))

  val l1: List[Int] = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  val l2: List[String] = Cons("foo", Cons("bar", Nil))
  val l3: List[Double] = Cons(2.0, Cons(4.0, Cons(8.0, Nil)))
  val l4: List[Int] = Cons(5, Cons(6, Cons(7, Nil)))
}
