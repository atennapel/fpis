// scala E13.scala

trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

trait Monad[F[_]] extends Functor[F] {
  def unit[A](a: => A): F[A]
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B]
  def map[A,B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: F[A], mb: F[B])(f: (A, B) => C): F[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))
}

sealed trait Free[F[_],A] {
  def flatMap[B](f: A => Free[F,B]) = FlatMap(this, f)
  def map[B](f: A => B) = flatMap(x => Return(f(x)))
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

object Free {
  
  trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
  type ~>[F[_], G[_]] = Translate[F,G]

  implicit def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
    def unit[A](a: => A): Free[F, A] = Return(a)
    def flatMap[A,B](ma: Free[F, A])(f: A => Free[F, B]): Free[F, B] = ma.flatMap(f)
  }

  @annotation.tailrec
  def step[F[_],A](x: Free[F,A]): Free[F,A] = x match {
    case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
    case FlatMap(Return(x), f) => step(f(x))
    case _ => x
  }

  @annotation.tailrec
  def runTrampoline[A](a: Free[Function0,A]): A =
    step(a) match {
      case Return(a) => a
      case Suspend(s) => s()
      case FlatMap(x, f) =>
        x match {
          case Suspend(r) => runTrampoline(f(r()))
          case _ => sys.error("Impossible: these cases should be eliminated by step")
        }
    }

  def run[F[_],A](a: Free[F,A])(implicit F: Monad[F]): F[A] =
    step(a) match {
      case Return(a) => F.unit(a)
      case Suspend(s) => s
      case FlatMap(x, f) =>
        x match {
          case Suspend(r) => F.flatMap(r)(a => run(f(a)))
          case _ => sys.error("Impossible: these cases should be eliminated by step")
        }
    }

  def runFree[F[_],G[_],A](free: Free[F,A])(t: F ~> G)(implicit G: Monad[G]): G[A] =
    step(free) match {
      case Return(a) => G.unit(a)
      case Suspend(r) => t(r)
      case FlatMap(Suspend(r),f) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible; `step` eliminates these cases")
    }

  def nat[F[_], G[_]](t: F ~> G): F ~> ({type f[a] = Free[G,a]})#f = new (F ~> ({type f[a] = Free[G,a]})#f) {
    def apply[A](f: F[A]): Free[G, A] = Suspend(t(f))
  }
  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = runFree(f)(nat(fg))

  sealed trait Console[A] {
    // def toPar: Par[A]
    def toThunk: () => A
  }
  case object ReadLine extends Console[Option[String]] {
    // def toPar = Par.lazyUnit(run)
    def toThunk = () => run
    def run: Option[String] =
      try Some("test")
      catch { case e: Exception => None }
  }
  case class PrintLine(line: String) extends Console[Unit] {
    // def toPar = Par.lazyUnit(println(line))
    def toThunk = () => println(line)
  }

  object Console {
    type ConsoleIO[A] = Free[Console, A]
    def readLn: ConsoleIO[Option[String]] =
      Suspend(ReadLine)
    def printLn(line: String): ConsoleIO[Unit] =
      Suspend(PrintLine(line))
  }

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
    () => f(a())()
  }

  val consoleToFunction0 =
    new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }

  def runConsoleFunction0[A](a: Free[Console,A]): () => A =
    runFree[Console,Function0,A](a)(consoleToFunction0)

  def runConsole[A](a: Free[Console,A]): A = runTrampoline(translate(a)(consoleToFunction0))

}
