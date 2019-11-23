package pl.marcinchwedczuk.fpinscala.chp2

import scala.annotation.tailrec

object FibonacciRecursive {
  def fib(n: Int): Long = {
    @tailrec
    def go(curr: Long, next: Long, index: Long): Long = {
      if (index >= n) curr
      else go(next, curr+next, index+1)
    }

    go(0, 1, 0)
  }

  def main(args: Array[String]): Unit = {
    for(i <- 0 to 10) {
      println(fib(i))
    }
  }
}
