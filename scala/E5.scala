// scala E5.scala
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

sealed trait Stream[+A] {

  import Stream._

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case s if n <= 0 => s
    case Cons(h, t) => t().drop(n - 1)
    case s => s
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => {
      val v = h()
      if (p(v)) cons(v, t().takeWhile(p)) else empty
    }
    case s => s
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((v, a) => p(v) && a)

  def takeWhile2(p: A => Boolean): Stream[A] = foldRight(this)((v, a) => if (p(v)) cons(v, a) else a)

  def headOption: Option[A] = foldRight[Option[A]](None)((v, a) => Some(v))

  def map[B](f: A => B): Stream[B] = foldRight[Stream[B]](empty)((v, a) => cons(f(v), a))

  def filter[B](f: A => Boolean): Stream[A] = foldRight[Stream[A]](empty)((v, a) => if (f(v)) cons(v, a) else a)

  def append[B >: A](other: Stream[B]): Stream[B] = foldRight[Stream[B]](other)((v, a) => cons(v, a))

  def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight[Stream[B]](empty)((v, a) => f(v).append(a))

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold((n, this)) {
    case (n, _) if n <= 0 => None
    case (n, Empty) => None
    case (n, Cons(h, t)) => Some(h(), (n - 1, t()))
  }

  def takeWhile3(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) => {
      val v = h()
      if (p(v)) Some(v, t()) else None
    }
    case _ => None
  }

  def zipWith[B, C](f: (A, B) => C)(other: Stream[B]): Stream[C] = unfold((this, other)) {
    case (Empty, _) => None
    case (_, Empty) => None
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
    case (Cons(h, t), s) => Some((Some(h()), None), (t(), s))
    case (s, Cons(h, t)) => Some((None, Some(h())), (s, t()))
    case (_, _) => None
  }

  def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) => h1() == h2() && t1().startsWith(t2())
    case (_, Empty) => true
    case (Empty, Cons(_, _)) => false
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(h, t) => Some(s, t())
    case Empty => None
  }

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this match {
      case Cons(h, t) => {
        val tail = t().scanRight(z)(f)
        tail.headOption match {
          case None => Empty
          case Some(x) => cons(f(h(), x), tail)
        }
      }
      case Empty => cons(z, empty)
    }

}

object Stream {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  
  def empty[A]: Stream[A] = Empty
  
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def fibsR(a: Int, b: Int): Stream[Int] = cons(a, fibsR(b, a + b))
    fibsR(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((v, z)) => cons(v, unfold(z)(f))
    }

  def ones2: Stream[Int] = unfold(())(x => Some(1, ()))

  def constant2[A](a: A): Stream[A] = unfold(())(x => Some(a, ()))

  def from2(n: Int): Stream[Int] = unfold(n)(n => Some(n, n + 1))

  def fibs2: Stream[Int] = unfold((0, 1)) { case (a, b) => Some(a, (b, a + b)) }

}
