// scala E2_1.scala

object E2_1 {
  def fib(n: Int): Int = {
    @scala.annotation.tailrec
    def fibR(n: Int, a: Int, b: Int): Int =
      if (n <= 0) a else fibR(n - 1, b, a + b)
    fibR(n, 0, 1)
  }

  def main(args: Array[String]) = println(1 to 10 map fib)
}
