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
  }
}
