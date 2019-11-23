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

  def foldRight[A,B](as: List[A], zero: B)(f: (A, B) => B): B = {
    // ... (N-2, (N-1, N))
    def go(as: List[A]): B = {
      as match {
        case Nil => zero
        case Cons(h, t) => f(h, go(t))
      }
    }

    go(as)
  }

  def foldLeft[A,B](zero: B, as: List[A])(f: (B, A) => B): B = {
    // ((1, 2), 3) ...

    def go(curr: B, as: List[A]): B = {
      as match {
        case Nil => curr
        case Cons(h, t) => go(f(curr, h), t)
      }
    }

    go(zero, as)
  }

  def mkString[A](as: List[A]): String  = {
    "[" + foldRight(as, "]") { _.toString + ", " + _ }
  }

  def foldRightLazy[A,B](as: List[A], zero: B)(f: (A, =>B) => B): B = {
    // ... (N-2, (N-1, N))
    def go(as: List[A]): B = {
      println(s"called go: ${mkString(as)}")
      as match {
        case Nil => zero
        case Cons(h, t) => f(h, go(t))
      }
    }

    go(as)
  }

  def productLazy(as: List[Int]): Int = {
    foldRightLazy(as, 0) { (curr, p) =>
      if (curr == 0) 0
      else curr * p
    }
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

    println("init:")
    println(init(init(list)))
    println(init(List(1)))

    println("foldLeft:")
    println(foldLeft(0, List[Int]()) { _ + _ })
    println(foldLeft(100, List(2, 5, 10)) { _ / _ })

    println("foldRight:")
    println(foldRight(List[Int](), 0)  { _ + _ })
    println(foldRight(List(2, 5, 10), 100) { (a, acc) => acc / a })
    println(foldRight(List(2.0, 3.0), 2.0)(Math.pow))

    println("product lazy:")
    println(productLazy(List(1,2,3,0,1,2,3)))
    println(productLazy(List(5,7,8)))
  }
}
