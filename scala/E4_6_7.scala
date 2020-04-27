// scala E4_6_7.scala

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

sealed trait Either[+E, +A] {

  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(_) => this.asInstanceOf[Either[E, B]]
      case Right(x) => Right(f(x))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => this.asInstanceOf[Either[EE, B]]
      case Right(x) => f(x)
    }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_) => b
      case Right(_) => this.asInstanceOf[Either[EE, B]]
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    (this, b) match {
      case (Right(x), Right(y)) => Right(f(x, y))
      case (Left(e), _) => this.asInstanceOf[Either[EE, C]]
      case (_, Left(e)) => b.asInstanceOf[Either[EE, C]]
    }

}

object Either {

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case x :: xs =>
        for (
          hd <- f(x);
          tl <- traverse(xs)(f)
        ) yield hd :: tl
    }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(x => x)

}
