// scala E3.scala

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def tail[T](l: List[T]): List[T] =
    l match {
      case Nil => Nil
      case Cons(_, t) => t
    }

  def setHead[T](l: List[T], h: T): List[T] =
    l match {
      case Nil => Nil
      case Cons(_, t) => Cons(h, t)
    }

  @annotation.tailrec
  def drop[T](l: List[T], n: Int): List[T] =
    (l, n) match {
      case (Nil, _) => Nil
      case (l, 0) => l
      case (Cons(_, t), n) => drop(t, n - 1)
    }

  def dropWhile[T](l: List[T], f: T => Boolean): List[T] = {
    @annotation.tailrec
    def loop(l: List[T]): List[T] =
      l match {
        case Cons(h, t) if f(h) => loop(t)
        case _ => l
      }

    loop(l)
  }

  // I was unable to write this tail-recursively nicely
  def init[T](l: List[T]): List[T] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  
  def length[T](l: List[T]): Int = foldRight(l, 0)((_, a) => a + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    @annotation.tailrec
    def loop(l: List[A], a: B): B =
      l match {
        case Nil => a
        case Cons(h, t) => loop(t, f(a, h))
      }

    loop(as, z)
  }

  def sum(l: List[Int]): Int = foldLeft(l, 0)(_ + _)
  def product(l: List[Int]): Int = foldLeft(l, 1)(_ * _)
  def length2[T](l: List[T]): Int = foldLeft(l, 0)((a, _) => a + 1)

}
