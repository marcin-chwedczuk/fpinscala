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

  def take2(n: Int): Stream[A] = {
    Stream.unfold[A, (Int, Stream[A])]((n, this)) {
      case (_, Empty) => None
      case (n, _) if n <= 0 => None
      case (n, Cons(h, t)) =>
        val el = h()
        val newState = (n-1, t())
        Some((el, newState))
    }
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

  final def takeWhile3(f: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Empty => None
      case Cons(h, t) =>
        if (f(h())) Some((h(), t()))
        else Some((h(), Stream.empty))
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

  def map2[B](f: A => B): Stream[B] = {
    this.foldRight(Stream.empty[B]) { (el, mappedTail) =>
      Stream.cons(f(el), mappedTail)
    }
  }

  def map3[B](f: A => B): Stream[B] = {
    Stream.unfold[B, Stream[A]](this) {
      case Empty => None
      case Cons(h, t) => Some((f(h()), t()))
    }
  }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) =>
      if (f(h())) Stream.cons(h(), t().filter(f))
      else t().filter(f)
  }

  def filter2(f: A => Boolean): Stream[A] = {
    this.foldRight(Stream.empty[A]) { (el, filteredTail) =>
      if (f(el)) Stream.cons(el, filteredTail)
      else filteredTail
    }
  }

  def append[B >: A](other: => Stream[B]): Stream[B] = this match {
    case Empty => other
    case Cons(h, t) =>
      Stream.cons(h(), t().append(other))
  }

  def append2[B >: A](other: => Stream[B]): Stream[B] = {
    this.foldRight(other) { (el, tail) =>
      Stream.cons(el, tail)
    }
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Empty => Empty
    case Cons(h, t) => f(h()).append(t().flatMap(f))
  }

  def flatMap2[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(Stream.empty[B]) { (s, flatMapped) =>
      f(s).append2(flatMapped)
    }
  }

  def startsWith[B >: A](bs: Stream[B]): Boolean = {
    Stream
      .zipAll(this, bs)((_, _))
      .foldRight(true) { (pair, acc) =>
        pair match {
          case (Some(aa), Some(bb)) if aa == bb => acc
          case (_, None) => true
          case _ => false
        }
      }
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s@Cons(_, t) => Some((s, t()))
    }
  }

  def hasSubsequence[B >: A](ss: Stream[B]): Boolean = {
    this
      .tails
      .exists(t => t.startsWith(ss))
  }

  def scanRight[B](zero: B)(f: (A, =>B) => B): Stream[B] = {
    this.foldRight((zero, Stream(zero))) { (el, pair) =>
      // pair (accumulator, scanList)
      lazy val tmp = pair
      val newAcc = f(el, tmp._1)
      val newOps = Stream.cons(newAcc, tmp._2)

      (newAcc, newOps)
    }._2
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

  def unfold[A, S](initialState: S)(f: S => Option[(A, S)]): Stream[A] = {
    def unfold(state: S): Stream[A] = {
      f(state) match {
        case Some((element, newState)) => Stream.cons(element, unfold(newState))
        case None => Stream.empty[A]
      }
    }

    unfold(initialState)
  }

  def ones: Stream[Int] = unfold(()) { s => Some((1, s)) }
  def constant(n: Int): Stream[Int] = unfold(n) { n => Some((n, n)) }
  def from(n: Int): Stream[Int] = unfold(n) { n => Some((n, n+1)) }
  def fibs: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0+f1))) }

  def zipWith[A,B,C](a: Stream[A], b: Stream[B])(f: (A,B) => C): Stream[C] = {
    Stream.unfold((a, b)) {
      case (Empty, _) => None
      case (_, Empty) => None
      case (Cons(ah, at), Cons(bh, bt)) =>
        val el = f(ah(), bh())
        val newState = (at(), bt())
        Some((el, newState))
    }
  }

  def zipAll[A,B,C](a: Stream[A], b: Stream[B])
                   (f: (Option[A],Option[B]) => C): Stream[C] = {
    Stream.unfold((a, b)) {
      case (Empty, Empty) => None

      case (Cons(ah, at), Empty) =>
        val el = f(Some(ah()), None)
        val newState = (at(), Empty)
        Some((el, newState))

      case (Empty, (Cons(bh, bt))) =>
        val el = f(None, Some(bh()))
        val newState = (Empty, bt())
        Some((el, newState))

      case (Cons(ah, at), Cons(bh, bt)) =>
        val el = f(Some(ah()), Some(bh()))
        val newState = (at(), bt())
        Some((el, newState))
    }
  }
}

object StreamProgram {
  def main(args: Array[String]): Unit = {
    import Stream._

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
    val ones: Stream[Int] = constant(1)
    val twos: Stream[Int] = constant(2)

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
    // println(ones.filter(_ > 3))
    println(sOneToTen.filter(_ % 2 == 0).toList)

    println("append:")
    println(ones.append(sOneToTen).take(3))
    println(sOneToTen.append(sOneToTen).append(ones).drop(7).take(7))

    println("flatMap:")
    println(sOneToTen.flatMap(x => Stream(x, x, x)).take(30))

    // map2/filter2/append2/flatMap2
    println("map2:")
    println(ones.map2(_*3).take(7))
    println("map3:")
    println(ones.map3(_*3).take(7))

    println("filter2:")
    println(ones.filter2(n => true).take(3))
    println(sOneToTen.filter2(_ % 2 == 0).toList)

    println("append2:")
    println(ones.append2(twos).take(3))
    println(sOneToTen.filter(_ > 7).append2(sOneToTen).take(7))

    println("flatMap2:")
    println(sOneToTen.flatMap2(constant).take(3))
    println(sOneToTen
      .flatMap2(n => Stream.cons(2*n, Stream.cons(3*n, Stream.empty)))
      .take(7))

    println("from:")
    println(from(3).take(10))

    println("fibs:")
    println(fibs.take(10))

    println("take2:")
    println(sOneToTen.take2(4).toList)
    println(twos.take2(4).toList)

    println("takeWhile3:")
    println(fibs.takeWhile3(_ < 10).toList)

    println("zipWith:")
    println(Stream.zipWith(sOneToTen, twos) { _ * _ }.take(8))

    println(Stream.zipAll(sOneToTen, twos.take2(5)) { (a,b) =>
      (for (aa <- a; bb <- b) yield aa*bb).getOrElse(-1)
    }.toList)

    println("startsWith:")
    println(Stream(1,2,3,4).startsWith(Stream(1,2,3,4)))
    println(Stream(1,2,3,4).startsWith(Stream(1,2,3,4,5)))
    println(Stream(1,2,3,4).startsWith(Stream(1,2,3)))
    println(Stream(1,2,3,4).startsWith(Stream(1,3,3)))

    sOneToTen.tails
      .map3(t => t.toList)
      .map3 { t => println(t); () } // Booo... side effect
      .toList // run it

    println(
      sOneToTen.hasSubsequence(Stream(4,5,6))
    )

    println(
      sOneToTen.hasSubsequence(Stream(1,4))
    )


    println(
      sOneToTen.scanRight(0)(_ + _).toList
    )
  }
}
