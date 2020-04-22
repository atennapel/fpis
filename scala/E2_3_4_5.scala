// scala E2_3_4_5.scala

object E2_3_4_5 {
  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    x => f(g(x)) 

  def main(args: Array[String]) = {}
}
