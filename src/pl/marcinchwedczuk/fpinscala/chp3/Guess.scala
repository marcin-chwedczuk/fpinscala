package pl.marcinchwedczuk.fpinscala.chp3

import scala.annotation.tailrec

object Guess {
  def main(args: Array[String]): Unit = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    println(x)
    println(sum(Cons(1, Cons(100, Nil))))
  }

  def sum(xs: List[Int]): Int = {
    @tailrec
    def go(xs: List[Int], curr: Int): Int = {
      xs match {
        case Nil => curr
        case Cons(h, t) => go(t, curr + h)
      }
    }

    go(xs, 0)
  }
}
