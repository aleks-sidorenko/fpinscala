package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def toList: List[A] = {
    this match {
      case Cons(h, t) => h() :: t().toList
      case _          => Nil
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) =>
        f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = unfold(n -> this) {
    case (i, Cons(t, h)) if i > 0 => Some(t(), (i - 1, h()))
    case (0, _) | (_, Empty) => None 
  }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else
      this match {
        case Cons(_, t) => t().drop(n - 1)
        case Empty      => empty
      }

  def takeWhile(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(t, h) if p(t()) => Some(t(), h())
    case _ => None 
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, acc) => acc && p(a))

  def headOption: Option[A] = this match {
    case Cons(h, _) => Some(h())
    case Empty      => None
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) {
      case (a, b) => if (p(a)) cons(a, b) else b
    }

  def map[B](f: A => B): Stream[B] = unfold(this) { 
    case Cons(h, t) => Some(f(h())-> t())
    case Empty => None
  }

  def append[A1 >: A](s: Stream[A1]): Stream[A1] =
    foldRight[Stream[A1]](s) {
      case (a, b) => cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty) {
      case (a, b) => f(a).append(b)
    }

  def zip[B](s2: Stream[B]): Stream[(A,B)] = unfold(this -> s2) {
    case (Cons(t1, h1), Cons(t2, h2)) => Some((t1() -> t2(), h1() -> h2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this -> s2) {
    case (Cons(t1, h1), Cons(t2, h2)) => Some((Some(t1()) -> Some(t2()), h1() -> h2()))
    case (Cons(t1, h1), Empty) => Some((Some(t1()) -> None, h1() -> Empty))
    case (Empty, Cons(t2, h2)) => Some((None -> Some(t2()), Empty -> h2()))
    case _ => None
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = !(this.zipAll(s).takeWhile { 
    case (_, Some(b)) => true
    case _ => false
  } exists { 
    case (Some(a), Some(b)) if a != b => true
    case (None, Some(_)) => true
    case _ => false
  })


}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = unfold(1) { case 1 => Some((1, 1))}

  def constant[A](a: A) : Stream[A] = unfold(a) { case i => Some((i, i))}

  def from(n: Int): Stream[Int] = unfold(n) { case i => Some((i, i + 1))}

  def fib: Stream[Int] = unfold((0, 1)) { case (p, c) => Some((c + p), (c, c + p))}

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    def loop(s: S): Stream[A] = f(s) match {
      case Some((a, ns)) => cons(a, loop(ns))
      case _ => empty
    }
    loop(z)
  }

  
}
