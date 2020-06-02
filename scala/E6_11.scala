// scala E6_11.scala

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

  def getState[S]: State[S, S] = State(s => (s, s))

  def put[S](s: S): State[S, Unit] = State(_ => ((), s))

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

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {

  def simulateInputForState(i: Input, state: Machine): Machine =
    (i, state) match {
      case (_, Machine(_, 0, _)) => state
      case (Turn, Machine(true, _, _)) => state
      case (Coin, Machine(false, _, _)) => state
      case (Coin, Machine(true, c, coins)) =>
        Machine(false, c, coins + 1)
      case (Turn, Machine(false, c, coins)) =>
        Machine(true, c - 1, coins)
    }

  def simulateInput(i: Input): State[Machine, Unit] =
    for (
      state <- State.getState;
      _ <- State.put(simulateInputForState(i, state))
    ) yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for (
      sim <- State.sequence(inputs.map(simulateInput(_)));
      state <- State.getState
    ) yield (state.coins, state.candies)

}
