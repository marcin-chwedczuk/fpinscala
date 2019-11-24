package pl.marcinchwedczuk.fpinscala.chp5

import scala.annotation.tailrec

sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  def toList: List[A] = {
    def go(as: Stream[A]): List[A] = as match {
      case Empty => List()
      case Cons(h, t) => h() :: go(t())
    }

    go(this)
  }

  def take(n: Int): List[A] = this match {
    case _ if (n <= 0) => List()
    case Empty => List()
    case Cons(h, t) => h() :: t().take(n-1)
  }

  @tailrec
  final def drop(n: Int): Stream[A] = this match {
    case s if (n <= 0) => s
    case Empty => Empty
    case Cons(_, t) => t().drop(n-1)
  }

  final def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Empty => Stream.empty[A]
    case Cons(h, t) =>
      if (f(h())) Stream.cons(h(), t().takeWhile(f))
      else Stream.empty[A]
  }

  def takeWhileAlt(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A]) { (h, tail) =>
      if (f(h)) Stream.cons(h, tail)
      else Stream.empty[A]
    }
  }

  @tailrec
  final def exists(p: A => Boolean): Boolean = this match {
    case Empty => false
    case Cons(h, t) => p(h()) || t().exists(p)
  }

  def foldRight[B](zero: B)(f: (A, =>B) => B): B = this match {
    case Empty => zero
    case Cons(h, t) => f(h(), t().foldRight(zero)(f))
  }

  @tailrec
  final def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, t) => p(h()) && t().forAll(p)
  }

  def headOptionAlt: Option[A] = {
    this.foldRight(None: Option[A]) { (v, _) =>
      Some(v)
    }
  }

  def map[B](f: A => B): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => Stream.cons(f(h()), t().map(f))
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = this match {
    case Empty => other
    case Cons(h, t) =>
      Stream.cons(h(), t().append(other))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => f(h()).append(t().flatMap(f))
  }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (f(h())) Stream.cons(h(), t().filter(f))
      else t().filter(f)
  }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A,
                    t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](h: => A,
              t: => Stream[A]): Stream[A] = {
    lazy val head = h
    lazy val tail = t

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }
}

object StreamProgram {
  def main(args: Array[String]): Unit = {

    val sOneToTen = Stream(1,2,3,4,5,6,7,8,9,10)

    println(Stream(1,2,3,4).toList)
    println(sOneToTen.take(3))
    println(sOneToTen.drop(3).toList)

    /*
    // check drop does *not* eval dropped head elements
    lazy val faultyInt = { throw new RuntimeException("faultyInt") }: Int
    println(
      cons(faultyInt, cons(faultyInt, cons(3, empty[Int])))
        .drop(2).toList
    )
    */

    println("takeWhile:")
    println(sOneToTen.takeWhile(_ < 7).toList)

    // !WARN infinite
    var ones: Stream[Int] = null
    ones = Cons(() => 1, () => ones)

    println("foldRight:")
    println(ones.foldRight(0) { (i,sum) => 1 })

    println("forAll:")
    println(ones.forAll(_ > 5))

    println("takeWhileAlt:")
    println(ones.takeWhileAlt(_ > 5).toList)
    println(sOneToTen.takeWhileAlt(_ < 7).toList)

    println("headOptionAlt:")
    println(ones.headOptionAlt)
    println(Stream.empty[Int].headOptionAlt)
    println(sOneToTen.headOptionAlt)

    println("map:")
    println(sOneToTen.map(i => 2*i).toList)
    println(ones.map(i => 3*i).take(5))

    println("filter:")
    // println(ones.filter(_ > 3).headOption)
    println(sOneToTen.filter(_ % 2 == 0).toList)

    println("append:")
    println(ones.append(sOneToTen).take(3))
    println(sOneToTen.append(sOneToTen).append(ones).drop(7).take(7))

    println("flatMap:")
    println(sOneToTen.flatMap(x => Stream(x, x, x)).take(30))
  }
}
