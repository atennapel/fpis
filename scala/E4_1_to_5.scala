// scala E4_1_to_5.scala

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
    this match {
      case Some(x) => Some(f(x))
      case None => None
    }
  
  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(x) => x
      case None => default
    }

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    this match {
      case Some(_) => this
      case None => ob
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(x) => f(x)
      case None => None
    }

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(x) if f(x) => this
      case _ => None
    }

}

object Option {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap { m => mean(xs map { x => math.pow(x - m, 2) }) }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (Some(x), Some(y)) => Some(f(x, y))
      case _ => None
    }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs =>
        for (
          y <- x;
          ys <- sequence(xs)
        ) yield y :: ys
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil => Some(Nil)
      case x :: xs =>
        for (
          hd <- f(x);
          tl <- traverse(xs)(f)
        ) yield hd :: tl
    }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

}
