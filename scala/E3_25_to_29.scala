// scala E3_25_to_29.scala

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[T](t: Tree[T]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(a, b) => 1 + size(a) + size(b)
    }

  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(a, b) => maximum(a) max maximum(b)
    }

  def depth[T](t: Tree[T]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(a, b) => 1 + (depth(a) max depth(b))
    }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(a, b) => Branch(map(a)(f), map(b)(f))
    }
  
  def fold[A, B](t: Tree[A], i: A => B)(f: (B, B) => B): B =
    t match {
      case Leaf(x) => i(x)
      case Branch(a, b) => f(fold(a, i)(f), fold(b, i)(f))
    }

  def size2[T](t: Tree[T]): Int =
    fold(t, (_: T) => 1)((a, b) => 1 + a + b)

  def maximum2(t: Tree[Int]): Int =
    fold(t, (x: Int) => x)((a, b) => a max b)

  def depth2[T](t: Tree[T]): Int =
    fold(t, (_: T) => 0)((a, b) => 1 + a.max(b))

  def map2[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t, (x: A) => Leaf(f(x)): Tree[B])((a, b) => Branch(a, b))

}
