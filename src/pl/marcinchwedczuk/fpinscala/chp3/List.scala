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
    @tailrec
    def go(curr: B, as: List[A]): B = {
      as match {
        case Nil => curr
        case Cons(h, t) => go(f(curr, h), t)
      }
    }

    go(zero, as)
  }

  def foldLeftAlt[A,B](zero: B, as: List[A])(f: (B,A) => B): B = {
    val computation = foldRight(as, (x:B) => x) { (a, fACC) =>
      (acc: B) => fACC(f(acc, a))
    }

    computation(zero)
  }

  def foldRightAlt[A,B](as: List[A], zero: B)(f: (A, B) => B): B = {
    val computation = foldLeft((x:B) => x, as) { (fACC, a) =>
      (acc: B) => fACC(f(a, acc))
    }

    computation(zero)
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

  def length(as: List[Int]): Int =
    foldRight(as, 0)((_, len) => len+1)

  def lengthL(as: List[Int]): Int =
    foldLeft(0, as)((len, _) => len+1)

  def sumL(as: List[Int]): Int =
    foldLeft(0, as) { _ + _ }

  def productL(as: List[Int]): Int =
    foldLeft(1, as) { _ * _ }

  def append[A](as: List[A], bs: List[A]): List[A] = {
    foldRight(as, bs)(Cons(_, _))
  }

  def append[A](lists: List[A]*): List[A] = {
    foldRight(toList(lists), Nil:List[A])(append)
  }

  def toList[A](as: Seq[A]): List[A] = {
    as match {
      case Seq() => Nil
      case Seq(h, t@_*) => Cons(h, toList(t))
    }
  }

  def addOne(ints: List[Int]): List[Int] = {
    foldRight(ints, Nil:List[Int]) { (n, tail) =>
      Cons(n+1, tail)
    }
  }

  def d2s(ds: List[Double]): List[String] = {
    foldRight(ds, Nil:List[String]) { (d, tail) =>
      Cons(d.toString, tail)
    }
  }

  def map[A,B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, Nil:List[B]) { (a,bs) =>
      Cons(f(a), bs)
    }
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, Nil:List[A]) { (a, rs) =>
      if (f(a)) Cons(a, rs)
      else rs
    }
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B]) { (a, bs) =>
      append(f(a), bs)
    }
  }

  def filterAlt[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as) { a =>
      if (f(a)) List(a)
      else Nil
    }
  }

  def sum(as: List[Int], bs: List[Int]): List[Int] = {
    def go(as: List[Int], bs: List[Int]): List[Int]  = {
      as match {
        case Nil => Nil
        case Cons(ah, at) =>
          bs match {
            case Nil => Nil
            case Cons(bh, bt) =>
              Cons(ah + bh, go(at, bt))
          }
      }
    }

    go(as, bs)
  }

  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] = {
    def go(as: List[A], bs: List[B]): List[C]  = {
      as match {
        case Nil => Nil
        case Cons(ah, at) =>
          bs match {
            case Nil => Nil
            case Cons(bh, bt) =>
              Cons(f(ah, bh), go(at, bt))
          }
      }
    }

    go(as, bs)
  }

  @tailrec
  def startsWith[A](as: List[A], sub: List[A]): Boolean = {
    as match {
      case Nil =>
        sub match {
          case Nil => true
          case _   => false
        }
      case Cons(ah, at) =>
        sub match {
          case Nil => true
          case Cons(sh, st) =>
            if (ah == sh) startsWith(at, st)
            else false
        }
    }
  }

  def hasSubsequence[A](as: List[A], sub: List[A]): Boolean = {
    @tailrec
    def go(as: List[A]): Boolean = {
      as match {
        case _ if startsWith(as, sub) => true
        case Nil => false
        case Cons(_, t) => go(t)
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

    println("constructors:")
    println(foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _)))
    println(foldLeft(Nil:List[Int], List(1,2,3)) { (acc, curr) => Cons(curr, acc) })

    println("length:")
    println(length(List()))
    println(length(List(1, 2, 3)))

    println("productL:")
    println(productL(List[Int]()))
    println(productL(List(1,2,3)))

    println("foldLeft/RightAlt:")
    println(foldLeftAlt("", List("foo", "bar", "nyu")) { _ + "::" + _ })
    println(foldRightAlt(List("foo", "bar", "nyu"), "") { _ + "::" + _ })

    println("append:")
    val l = List(1,2,3)
    val appended = List(101,102,103)
    println(mkString(append(l, appended)))

    println("addOne:")
    println(mkString(addOne(l)))

    println("d2s:")
    println(mkString(d2s(List(1.0, 2.0, 3.0))))

    println("map:")
    pr(map(List[Int]()) { _*100 })
    pr(map(List(1,2,3)) { _*100 })

    println("filter:")
    pr(filter(List(1,2,3,4,5,6)){ _ % 2 == 0 })
    pr(filter(l)(a => false))
    pr(filter(l)(a => true))

    println("filterAlt:")
    pr(filterAlt(List(1,2,3,4,5,6)){ _ % 2 == 0 })
    pr(filterAlt(l)(a => false))
    pr(filterAlt(l)(a => true))

    pr(sum(List(1,2,3), List(1,2,3)))
    pr(zipWith(List(1,2,3,4), List(1,-2,3)) { _ + _ })

    println("hasSubsequence:")
    println(hasSubsequence(List(1,2,3), List(3)))
    println(hasSubsequence(List(1,2,3), List()))
    println(hasSubsequence(List(1,2,3), List(1,5)))
  }

  private def pr[A](as: List[A]): Unit = println(List.mkString(as))
}
