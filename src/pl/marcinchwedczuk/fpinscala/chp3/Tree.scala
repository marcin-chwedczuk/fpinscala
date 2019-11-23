package pl.marcinchwedczuk.fpinscala.chp3


sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }
  }

  def max(t: Tree[Int]): Int = {
    t match {
      case Leaf(v) => v
      case Branch(l, r) => max(l) `max` max(r)
    }
  }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + (depth(l) `max` depth(r))
    }
  }

  def map[A,B](as: Tree[A])(f: A => B): Tree[B] = {
    as match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(v) => f(v)
      case Branch(l, r) =>
        g(fold(l)(f)(g), fold(r)(f)(g))
    }
  }

  def mapAlt[A,B](as: Tree[A])(f: A => B): Tree[B] = {
    fold(as)(v => Leaf(f(v)):Tree[B])(Branch(_,_))
  }
}

object TreeProgram {
  def main(args: Array[String]): Unit = {
    import Tree._

    val t = Branch(
      Leaf(1),
      Branch(
        Leaf(2),
        Leaf(3)
      )
    )

    println(size(t))
    println(max(t))
    println(depth(t))
    println(map(t)(i => i+1))
    println(mapAlt(t)(i => i+1))
  }
}
