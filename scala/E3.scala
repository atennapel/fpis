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

  def reverse[T](l: List[T]): List[T] = foldLeft(l, Nil: List[T])((a, h) => Cons(h, a))

  def foldLeft2[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (x: B) => x)((b, g) => x => g(f(x, b)))(z)
  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (x: B) => x)((g, b) => x => g(f(b, x)))(z)

  def append[T](a: List[T], b: List[T]): List[T] =
    foldRight(a, b)(Cons(_, _))
  
  def flatten[T](xs: List[List[T]]): List[T] =
    foldLeft(xs, Nil: List[T])(append)

  def add1toEach(l: List[Int]): List[Int] =
    foldLeft(l, Nil: List[Int])((a, h) => Cons(h + 1, a))
  def convertToString(l: List[Double]): List[String] =
    foldLeft(l, Nil: List[String])((a, h) => Cons(h.toString, a))

  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldLeft(as, Nil: List[B])((a, h) => Cons(f(h), a))

  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldLeft(as, Nil: List[A])((a, h) => if (f(h)) Cons(h, a) else a)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  def zipAdd(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, zipAdd(t1, t2))
      case (_, _) => Nil
    }

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
      case (_, _) => Nil
    }

}
