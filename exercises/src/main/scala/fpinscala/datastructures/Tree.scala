package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    tree match {
      case null         => 0
      case Leaf(_)      => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def maximum(tree: Tree[Int]): Int = {
    def go(tree: Tree[Int], max: Int): Int =
      tree match {
        case null         => max
        case Leaf(v: Int) => math.max(max, v)
        case Branch(l, r) => math.max(go(l, max), go(r, max))
      }

    go(tree, 0)
  }

  def depth[A](tree: Tree[A]): Int = {
    def go(tree: Tree[_], acc: Int): Int =
      tree match {
        case null         => acc
        case Leaf(_)      => acc + 1
        case Branch(l, r) => math.min(go(l, acc + 1), go(r, acc + 1))
      }

    go(tree, 0)
  }

  def map[A, B](tree: Tree[A])(f: (A => B)): Tree[B] =
    tree match {
      case null         => null
      case Leaf(v)      => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

}
