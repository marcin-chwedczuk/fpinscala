package pl.marcinchwedczuk.fpinscala.chp2

import scala.annotation.tailrec

object IsSorted {
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @tailrec
    def go(curr: Int): Boolean = {
      if (curr <= 0) true
      else if (ordered(as(curr-1), as(curr))) go(curr-1)
      else false
    }

    go(as.length-1)
  }

  def main(args: Array[String]): Unit = {
    val cmp: (Int, Int) => Boolean = { _ <= _ }

    check(Array())
    check(Array(1))
    check(Array(1,2))
    check(Array(2,1))
    check(Array(1,2,3,2,5))
    check(Array(1,2,3,4,5))

    def check(as: Array[Int]) = {
      println(s"${as.mkString("[", ",", "]")} ${isSorted(as, cmp)}")
    }
  }
}
