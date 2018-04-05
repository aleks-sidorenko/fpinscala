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

  def take(n: Int): Stream[A] = {
    if (n == 0) empty
    else
      this match {
        case Cons(h, t) => Cons(h, () => t().take(n - 1))
        case Empty      => empty
      }
  }

  def drop(n: Int): Stream[A] =
    if (n == 0) this
    else
      this match {
        case Cons(_, t) => t().drop(n - 1)
        case Empty      => empty
      }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty) {
      case (a, b) => if (!p(a)) empty else cons(a, b)
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

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](empty) {
      case (a, b) => cons(f(a), b)
    }

  def append[A1 >: A](s: Stream[A1]): Stream[A1] =
    foldRight[Stream[A1]](s) {
      case (a, b) => cons(a, b)
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](empty) {
      case (a, b) => f(a).append(b)
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def startsWith[B](s: Stream[B]): Boolean = ???
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

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = ???

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = ???
}
