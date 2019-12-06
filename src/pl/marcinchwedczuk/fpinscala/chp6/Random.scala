package pl.marcinchwedczuk.fpinscala.chp6

import scala.annotation.tailrec

trait RNG {
  def nextInt: (Int, RNG)

  def nextPositiveInt: (Int, RNG) = {
    val (n, rng) = nextInt
    val p = if (n >= 0) n
            // clear sign bit
            else n & 0x7FFFFFFF

    (p, rng)
  }

  def double: (Double, RNG) = {
    val (p, rng) = nextPositiveInt
    val d = p / (Int.MaxValue + 1.0d)
    (d, rng)
  }

  def randID: ((Int, Double), RNG) = {
    val (i, rng) = nextInt
    val (d, rng2) = rng.double
    ((i, d), rng2)
  }

  def randDI: ((Double, Int), RNG) = {
    val (d, rng) = double
    val (i, rng2) = rng.nextInt
    ((d, i), rng2)
  }

  def randDDD: ((Double, Double, Double), RNG) = {
    val (d1, rng) = double
    val (d2, rng2) = rng.double
    val (d3, rng3) = rng2.double

    ((d1, d2, d3), rng3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(i: Int, rng: RNG, curr: List[Int]): (List[Int], RNG) = {
      if (i <= 0)
        (curr, rng)
      else {
        val (n, rng2) = rng.nextInt
        go(i-1, rng2, n :: curr)
      }
    }

    go(count, rng, List())
  }
}

case class SimpleRNG(seed: Long) extends RNG {
  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt

    (n, nextRNG)
  }
}

case class State[S,+A](run: S => (A,S)) {
  def map[B](f: A => B): State[S,B] = {
    State(s => {
      val (v, s2) = run(s)
      (f(v), s2)
    })
  }

  def flatMap[B](g: A => State[S,B]): State[S,B] = {
    State(s => {
      val (fv, s2) = run(s)
      g(fv).run(s2)
    })
  }
}

object State {
  def unit[S,A](a: A): State[S,A] = State(s => (a, s))

  def map2[S,A,B,C](a: State[S,A], b: State[S,B])(f: (A,B) => C): State[S,C] = {
    State(s => {
      val (av, s2) = a.run(s)
      val (bv, s3) = b.run(s2)

      (f(av, bv), s3)
    })
  }

  def sequence[S,A](fs: List[State[S,A]]): State[S, List[A]] = {
    fs.foldLeft(unit[S,List[A]](List())) { (rlist, ra) =>
      map2(rlist, ra) { (list, a) => a :: list }
    }
  }

  def main(args: Array[String]): Unit = {
    type R = State[RNG, Int]
    val r: RNG = SimpleRNG(143443)

    def int(): R = State(_.nextInt)
    def int2(max: Int): R = int().map(_ % max)

    println(sequence(List.fill(7)(int2(10))).run(r))
  }
}

object Random {
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (v, rng2) = s(rng)
      (f(v), rng2)
    }
  }

  def mapAlt[A,B](s: Rand[A])(f: A => B): Rand[B] = {
    flatMap(s)(f.andThen(unit))
  }

  def map2[A,B,C](a: Rand[A], b: Rand[B])(f: (A,B) => C): Rand[C] = {
    rng => {
      val (av, rng2) = a(rng)
      val (bv, rng3) = b(rng2)

      (f(av, bv), rng3)
    }
  }

  def map2Alt[A,B,C](a: Rand[A], b: Rand[B])(f: (A,B) => C): Rand[C] = {
    flatMap(a) { av =>
      flatMap(b) { bv =>
        unit(f(av, bv))
      }
    }
  }

  def both[A,B](a: Rand[A], b: Rand[B]): Rand[(A,B)] =
    map2(a, b)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    fs.foldLeft(unit(List[A]())) { (rlist, ra) =>
      map2(rlist, ra) { (list, a) => a :: list }
    }
  }

  def ints2(n: Int): Rand[List[Int]] = {
    sequence(List.fill(n)(int))
  }

  def nextInt(maxExcluded: Int): Rand[Int] =
    map(positiveInt){ _ % maxExcluded }

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (fv, rng2) = f(rng)
      g(fv)(rng2)
    }
  }

  def nextInt2(n: Int): Rand[Int] =
    flatMap(positiveInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod < 0) nextInt2(n)
      else unit(mod)
    }

  val positiveInt: Rand[Int] = _.nextPositiveInt

  def nonNegativeEven: Rand[Int] =
    map(positiveInt)(i => i - i % 2)

  def double: Rand[Double] = map(int)(i => i / (Int.MaxValue + 1.0d))

  def main(args: Array[String]): Unit = {
    val r = SimpleRNG(143443)
    println(r.nextInt._1)
    println(r.nextInt._2.nextInt._1)

    println(r.nextPositiveInt)
    println(r.double)

    println("random tuples:")
    println(r.randDDD._1)
    println(r.randDI._1)
    println(r.randID._1)

    println("ints:")
    println(r.ints(10)(r)._1)

    println("positive even:")
    println(nonNegativeEven(r)._1)

    println("double2:")
    println(double(r)._1)
    println(double(r)._1)

    println("map2:")
    println(map2(int, int)((_,_))(r))

    println("map2Alt:")
    println(map2Alt(int, int)((_,_))(r))

    println("mapAlt:")
    println(mapAlt(int)(i => s"foo$i")(r))

    println("ints2:")
    println(ints2(5)(r))

    println("nextInt2:")
    println(nextInt2(3)(r))
  }
}
