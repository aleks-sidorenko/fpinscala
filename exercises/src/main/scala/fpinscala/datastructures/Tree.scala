package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


  def size[A](tree: Tree[A]): Int = {
    tree match {
        case null => 0
        case Leaf(_) => 1
        case Branch(l, r) => 1 + size(l) + size(r)
      }
  }

  def maximum(tree: Tree[Int]): Int = {
    def go(tree: Tree[Int], max: Int): Int = 
      tree match {
          case null => max
          case Leaf(v: Int) => math.max(max, v)
          case Branch(l, r) => math.max(go(l, max), go(r, max))
        }
    
    go(tree, 0)
  }

}