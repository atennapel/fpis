// scala E2_2.scala

object E2_2 {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    val end = as.size - 1

    @annotation.tailrec
    def loop(i: Int): Boolean =
      if (i >= end) true else ordered(as(i), as(i + 1)) && loop(i + 1)

    loop(0)
  }

  def main(args: Array[String]) = {
    println(isSorted(Array(1, 2, 3, 4, 5), (a: Int, b: Int) => a <= b))
    println(isSorted(Array(1, 3, 2, 5, 4), (a: Int, b: Int) => a <= b))
  }
}
