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
        case Nil => Nil
        case Cons(h, t) => if (f(h)) loop(t) else l
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

}
