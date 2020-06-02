// scala E6_1_to_10.scala
trait RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {

  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }

}

object SimpleRNG {

  type Rand[+A] = RNG => (A, RNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n == Int.MinValue) 0 else n.abs, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = rng.nextInt
    (((n.toDouble / Int.MinValue.toDouble) + 1) / 2, rng2)
  }

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0)
      (Nil, rng)
    else {
      val (n, rng2) = rng.nextInt
      val (ns, rng3) = ints(count - 1)(rng2)
      (n :: ns, rng3)
    }

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)
 
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def double2: Rand[Double] =
    map(int)(x => ((x.toDouble / Int.MinValue.toDouble) + 1) / 2)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)
  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    rng => fs match {
      case Nil => (Nil, rng)
      case h :: t => {
        val (a, rng2) = h(rng)
        val (b, rng3) = sequence(t)(rng2)
        (a :: b, rng3)
      }
    }

  def ints2(count: Int)(rng: RNG): Rand[List[Int]] =
    sequence(List.fill(count)(()).map(x => int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, rng2) = f(rng)
      g(x)(rng2)
    }

}

case class State[S,+A](run: S => (A,S)) {

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s2) = run(s)
      (f(a), s2)
    })

  def map2[B,C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = run(s)
      val (b, s3) = rb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (x, s2) = this.run(s)
      g(x).run(s2)
    })

}

object State {

  def unit[S, A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    State(s => fs match {
      case Nil => (Nil, s)
      case h :: t => {
        val (a, s2) = h.run(s)
        val (b, s3) = sequence(t).run(s2)
        (a :: b, s3)
      }
    })

}
