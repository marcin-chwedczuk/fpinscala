package pl.marcinchwedczuk.fpinscala.chp3

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = {
    as match {
      case Cons(_, tail) => tail
      case Nil => throw new RuntimeException("Cannot get tail of empty list.")
    }
  }

  def replaceHead[A](as: List[A], newHead: A): List[A] = {
    Cons(newHead, tail(as))
  }

  def drop[A](as: List[A], n: Int): List[A] = {
    @tailrec
    def go(as: List[A], n: Int): List[A] = {
      as match {
        case _ if (n <= 0) => as
        case Nil => Nil
        case Cons(_, t) => go(t, n - 1)
      }
    }

    go(as, n)
  }

  def dropWhile[A](as: List[A], pred: A => Boolean): List[A] = {
    @tailrec
    def go(as: List[A]): List[A] = {
      as match {
        case Nil => Nil
        case Cons(h, t) =>
          if (pred(h)) go(t)
          else as
      }
    }

    go(as)
  }

  def init[A](as: List[A]): List[A] = {
    def go(as: List[A]): List[A] = {
      as match {
        case Nil => throw new RuntimeException("Cannot take init of empty list")
        case Cons(_, Nil) => Nil
        case Cons(h, t) => Cons(h, go(t))
      }
    }

    go(as)
  }
}

object Program {
  def main(args: Array[String]): Unit = {
    import List._

    val list = List(1,2,3,4)

    println(tail(list))
    println(replaceHead(list, 55))

    for (n <- 0 to 4) {
      println(drop(list, n))
    }

    println(dropWhile(list, (_: Int) => false))
    println(dropWhile(list, (_: Int) => true))
    println(dropWhile[Int](list, a => a < 3))

    println(init(init(list)))
    println(init(List(1)))
  }
}
