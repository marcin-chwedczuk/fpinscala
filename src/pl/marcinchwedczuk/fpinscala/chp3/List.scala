package pl.marcinchwedczuk.fpinscala.chp3

import pl.marcinchwedczuk.fpinscala.chp3.List.tail

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
}

object Program {
  def main(args: Array[String]): Unit = {
    val list = List(1,2,3,4)
    println(tail(list))
  }
}
