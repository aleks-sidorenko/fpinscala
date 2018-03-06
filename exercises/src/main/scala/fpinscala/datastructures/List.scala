package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int =
    foldLeft(ints, 0)(_ + _)

  def product(ds: List[Double]): Double =
    foldLeft(ds, 1.0)(_ * _)

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


  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2) { case (h, t) => Cons(h, t) }


  def flatten[A](lists: List[List[A]]): List[A] = 
    foldRight(lists, Nil: List[A]) { case (l, acc) => append(l, acc)}
    

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(_, xs) => xs
      case Nil => Nil
    }

  def setHead[A](l: List[A], h: A): List[A] = 
    Cons(h, tail(l))

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = 
    if (n <= 0) l
    else drop(tail(l), n - 1)

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =  
    l match {
      case Cons(x, xs) => if (f(x)) dropWhile(xs, f) else l
      case Nil => Nil
    }

  def init[A](l: List[A]): List[A] = 
    l match {
      case Cons(_, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => Nil
    }

  def length[A](l: List[A]): Int = 
    foldRight(l, 0)((_, acc) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = 
    l match {
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
      case Nil => z
    }

  def foldLeft2[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    type B2B = (B => B)
    val identity: B2B = (b: B) => b
    val g: (A, B2B) => B2B = (x, acc) => (b: B) => acc(f(b, x))
    val b2b = foldRight[A, B2B](l, identity) { case (x, acc) => 
      g(x, acc)
    }

    b2b(z)
  }

  def reverse2[A](l: List[A]): List[A] = 
    foldLeft2[A, List[A]](l, Nil) { case (acc, x) => Cons(x, acc) }

  def reverse[A](l: List[A]): List[A] = 
    foldLeft[A, List[A]](l, Nil) { case (acc, x) => Cons(x, acc) }
  
  def add1(l: List[Int]): List[Int] = 
    l match {
      case Nil => Nil
      case Cons(h, t) => Cons(h + 1, add1(t))
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
