package pl.marcinchwedczuk.fpinscala.chp6

trait RNG {
  def nextInt: (Int, RNG)

  def nextPositiveInt: (Int, RNG) = {
    val (n, rng) = nextInt
    val p = if (n >= 0) n
            // clear sign bit
            else n & 0x7FFFFFFF

    (p, rng)
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
  def main(args: Array[String]): Unit = {
    val r = SimpleRNG(42)
    println(r.nextInt._1)
    println(r.nextInt._2.nextInt._1)

    println(r.nextPositiveInt)
  }
}
