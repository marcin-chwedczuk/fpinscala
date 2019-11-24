package pl.marcinchwedczuk.fpinscala.chp4

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case Some(v) => Some(f(v))
      case None    => None
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case Some(v) => v
      case None => default
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this.map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
     this.map(v => Some(v)).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
    this.flatMap { v =>
      if (f(v)) Some(v)
      else None
    }
  }

  def mean(ds: Seq[Double]): Option[Double] = {
    if (ds.isEmpty) None
    else Some(ds.sum / ds.length)
  }

  def variance(ds: Seq[Double]): Option[Double] = {
    mean(ds).map { mean =>
      ds.map(v => math.pow(v - mean, 2)).sum
    }
  }

  def map2[B,C](b: Option[B])(f: (A,B) => C): Option[C] = {
    this.flatMap { av =>
      b.map(bv => f(av, bv))
    }
  }
}

case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {
    as.foldRight(Some(List()): Option[List[A]]) { (vOpt, lOpt) =>
      lOpt.flatMap { l =>
        vOpt.map(v => v :: l)
      }
    }
  }

  def traverse[A,B](as: List[A])(f: A => Option[B]): Option[List[B]] = {
    as.foldRight(Some(List()): Option[List[B]]) { (a, lOpt) =>
      lOpt.flatMap { l =>
        f(a).map(fa => fa :: l)
      }
    }
  }

  def sequenceAlt[A](as: List[Option[A]]): Option[List[A]] =
    traverse(as)(x => x)
}

object OptionProgram {
  def main(args: Array[String]): Unit = {
    val some1: Option[Int] = Some(1)
    val some2: Option[Int] = Some(2)
    val none: Option[Int] = None

    println("map:")
    println(some1.map(i => 2*i))
    println(none.map(i => 2*i))

    println("flatMap:")
    println(some1.flatMap(i => None))
    println(some1.flatMap(i => Some(13)))
    println(none.flatMap(i => Some(42)))

    println("getOrElse:")
    println(some1.getOrElse(101))
    println(none.getOrElse(102))

    println("orElse:")
    println(some1.orElse(some2))
    println(none.orElse(some2))

    println("filter:")
    println(some1.filter(i => i > 100))
    println(some1.filter(i => i <= 100))

    println("map2:")
    println(some1.map2(some2)(_ + _))
    println(none.map2(some2)(_ + _))
    println(some1.map2(none)(_ + _))

    println("sequence:")
    println(Option.sequence(List(some1, some1, some2)))
    println(Option.sequence(List(none, some1, some2)))
    println(Option.sequence(List(some1, some1, none)))

    println("sequenceAlt:")
    println(Option.sequenceAlt(List(some1, some1, some2)))
    println(Option.sequenceAlt(List(none, some1, some2)))
    println(Option.sequenceAlt(List(some1, some1, none)))

    println("tranverse:")
    println {
      Option.traverse(List(1, 2, 3, 4, 5, 6, 7))(v => Some(v*2))
    }
  }
}
