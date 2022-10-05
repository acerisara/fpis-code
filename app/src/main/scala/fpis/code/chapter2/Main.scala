package fpis.code.chapter2

import scala.annotation.tailrec

object Main {

  def fib(n: Int): Int = {
    @tailrec
    def go(n: Int, previous: Int, current: Int): Int =
      if (n == 0) previous
      else go(n - 1, current, previous + current)

    go(n, 0, 1)
  }

  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def go(as: Array[A], i: Int): Boolean =
      if (i >= as.length - 1) true
      else if (ordered(as(i), as(i + 1))) go(as, i + 1)
      else false

    go(as, 0)
  }

}
