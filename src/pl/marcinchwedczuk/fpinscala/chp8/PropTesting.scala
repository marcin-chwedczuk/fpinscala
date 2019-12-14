package pl.marcinchwedczuk.fpinscala.chp8

import pl.marcinchwedczuk.fpinscala.chp6.{RNG, SimpleRNG, State}
import pl.marcinchwedczuk.fpinscala.chp8.Prop.{FailedCase, SuccessCount}

trait Prop { self =>
  def check(): Either[(FailedCase, SuccessCount), SuccessCount]

  def &&(other: Prop): Prop = {
    new Prop {
      override def check(): Either[(FailedCase, SuccessCount), SuccessCount] =  {
        self.check() match {
          case Right(s) =>
            other.check() match {
              case Right(o) => Right(s + o)
              case Left((f, o)) => Left((f, s + o))
            }
          case l@Left(_) => l
        }
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int

}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = {
    Gen[B](sample.map(f))
  }
}

object Gen {
  def unit[A](a: A): Gen[A] = {
    Gen(State.unit(a))
  }

  def map2[A,B,C](a: Gen[A], b: Gen[B])(f: (A,B) => C): Gen[C] = {
    Gen(State.map2(a.sample, b.sample)(f))
  }

  def sequence[A](fs: List[Gen[A]]): Gen[List[A]] = {
    fs.foldRight(unit(List[A]())) { (el, list) =>
      map2(el, list)(_ :: _)
    }
  }

  def generated[A](a: => A): Gen[A] = {
    Gen[A](State.unit(()).map(_ => a))
  }

  private def randNonNegativeInt: State[RNG, Int] = State[RNG, Int](_.nextNonNegativeInt)

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    val sample = randNonNegativeInt
      .map(n => start + (n % (stopExclusive - start)))

    Gen(sample)
  }

  def boolean: Gen[Boolean] = {
    choose(0, 2).map {
      case 0 => false
      case 1 => true
    }
  }

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    sequence(List.fill(n)(a))
  }

  def listOf[A](element: Gen[A]): Gen[List[A]] = {
    ???
  }

  def forAll[A](a: Gen[A])(p: A => Boolean): Prop = {
    ???
  }
}

object PropTesting {

  def main(args: Array[String]): Unit = {
    ps("unit", Gen.unit(1))

    var n = 0
    ps("generated", Gen.generated { n += 1; n })

    ps("choose", Gen.choose(0, 7))

    ps("listOfN", Gen.listOfN(4, Gen.choose(0, 5)))

    ps("boolean", Gen.boolean)
  }

  private def ps[A](name: String, g: Gen[A]): Unit = {
    val rng: RNG = SimpleRNG(1)
    print(s"$name: ")
    val genList = Gen.listOfN(10, g).sample.run(rng)._1
    println(s"$genList")
  }
}
