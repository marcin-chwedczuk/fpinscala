package pl.marcinchwedczuk.fpinscala.chp2

object Currying {
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => (b => f(a,b))
  }

  def uncurry[A,B,C](f: A => (B => C)): (A,B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    val curried = curry(fun)
    println(curried(3)(true))

    val uncurried = uncurry(curried)
    println(uncurried(3, true))

    val composed = compose(dup, toInt)
    println(composed("3"))
  }

  def fun(n: Int, b: Boolean): String = {
    s"$n $b"
  }

  def toInt(s: String): Int = s.toInt
  def dup(n: Int): (Int, Int) = (n, n)
}
