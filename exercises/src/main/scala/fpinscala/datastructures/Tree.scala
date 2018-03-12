package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = {
    fold(tree, 0)({ case (acc, _) => 1 }, {
      case (l, r)                 => 1 + l + r
    })
  }

  def maximum(tree: Tree[Int]): Int = {
    fold(tree, 0)({ case (_, v) => v }, math.max)
  }

  def depth[A](tree: Tree[A]): Int = {
    fold(tree, 0)({ case (acc, _) => 1 }, {
      case (l, r)                 => 1 + math.max(l, r)
    })
  }

  def map[A, B](tree: Tree[A])(f: (A => B)): Tree[B] =
    fold(tree, null: Tree[B])({ case (_, x) => Leaf(f(x)) }, {
      case (l, r)                           => Branch(l, r)
    })

  def fold[A, B](tree: Tree[A], z: B)(f: ((B, A) => B), g: ((B, B) => B)): B =
    tree match {
      case null         => z
      case Leaf(v)      => f(z, v)
      case Branch(l, r) => g(fold(l, z)(f, g), fold(r, z)(f, g))
    }

}
