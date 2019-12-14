package pl.marcinchwedczuk.fpinscala.chp8

import pl.marcinchwedczuk.fpinscala.chp6.{RNG, SimpleRNG, State}
import pl.marcinchwedczuk.fpinscala.chp8.Prop.{FailedCase, MaximumSize, SuccessCount, TestCases}
import pl.marcinchwedczuk.fpinscala.chp5._

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase,
                     successCount: SuccessCount) extends Result {
  override def isFalsified: Boolean = true

  override def toString: FailedCase =
    s"Falsified { failingCase=$failure, attempts=$successCount }"
}

// testCases - number of tries per *single* prop!
case class Prop(run: (MaximumSize, TestCases, RNG) => Result) { self =>
  def &&(other: Prop): Prop = {
    Prop { case (maxSize, testCases, rng) =>
      self.run(maxSize, testCases, rng) match {
        case f: Falsified => f
        case Passed =>
          other.run(maxSize, testCases, rng) match {
            case f: Falsified => f
            case p@Passed => p
          }
      }
    }
  }

  def ||(other: Prop): Prop = {
    Prop { case (maxSize, testCases, rng) =>
      self.run(maxSize, testCases, rng) match {
        case p@Passed => p
        case _: Falsified =>
          other.run(maxSize, testCases, rng) match {
            case p@Passed => p
            case f: Falsified => f
          }
      }
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaximumSize = Int

  def forAll[A](a: SGen[A])(p: A => Boolean): Prop = Prop { (size, n, rng) =>
    val casesPerSize = (n + (size - 1)) / size

    val prop = Stream.from(0)
      .take((n min size) + 1)
      .map { sampleSize =>
        forAll(a.forSize(sampleSize))(p)
      }
      .map { p =>
        Prop { (_size, _, _rng) =>
          p.run(_size, casesPerSize, _rng)
        }
      }
      .reduce(_ && _)

    prop.run(size, n, rng)
  }

  def forAll[A](a: Gen[A])(p: A => Boolean): Prop = Prop { (_, n, rng) =>
    Stream.zipWith(
        randomStream(a)(rng),
        Stream.from(0))((_, _))
      .take(n).map { case (a, i) =>
        try { if (p(a)) Passed else Falsified(a.toString, i) }
        catch { case e: Exception => Falsified(buildMsg(a, e), i) }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  private def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] = {
    Stream.unfold(rng)(r => Some(g.sample.run(r)))
  }

  private def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.toString}\n" +
    s"stacktrace:\n${e.getStackTrace.mkString("\n")}"
}

case class Gen[+A](sample: State[RNG, A]) { self =>
  def map[B](f: A => B): Gen[B] = {
    Gen[B](sample.map(f))
  }

  def flatMap[B](f: A => Gen[B]): Gen[B] = {
    Gen(sample.map(f).flatMap(_.sample))
  }

  def unsized: SGen[A] = {
    SGen(_ => self)
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

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = {
    weighted((g1, 0.5), (g2, 0.5))
  }

  def weighted[A](g1: (Gen[A],Double), g2: (Gen[A],Double)): Gen[A] = {
    val (gen1, w1) = g1
    val (gen2, w2) = g2

    val w = w1 + w2

    Gen[A](State { rng =>
      val (d, rng2) = rng.double
      if (d*w < w1)
        gen1.sample.run(rng2)
      else
        gen2.sample.run(rng2)
    })
  }

  def boolean: Gen[Boolean] = {
    choose(0, 2).map {
      case 0 => false
      case 1 => true
    }
  }

  def lowerCaseLetter: Gen[Char] = {
    choose('a'.toInt, 'z'.toInt+1).map(_.toChar)
  }

  def word(minLength: Int, maxLengthExclusive: Int): Gen[String] = {
    choose(minLength, maxLengthExclusive)
      .flatMap { len =>
        listOfN(len, lowerCaseLetter)
      }
      .map(_.mkString(""))
  }

  def word: Gen[String] = word(3, 10)

  def sentence: Gen[String] = {
    choose(3, 8)
      .flatMap { wordsNumber =>
        listOfN(wordsNumber, word)
      }
      .map(_.mkString(" "))
      .map(_.capitalize.concat("."))
  }

  def listOfN[A](n: Int, a: Gen[A]): Gen[List[A]] = {
    listOfN(Gen.unit(n), a)
  }

  def listOfN[A](n: Gen[Int], a: Gen[A]): Gen[List[A]] = {
    n.flatMap { size =>
      sequence(List.fill(size)(a))
    }
  }

  def listOf[A](element: Gen[A]): Gen[List[A]] = {
    ???
  }
}

case class SGen[+A](forSize: Int => Gen[A]) { self =>
  def map[B](f: A => B): SGen[B] = {
    SGen[B](size => self.forSize(size).map(f))
  }

  def flatMap[B](f: A => Gen[B]): SGen[B] = {
    SGen[B](size => self.forSize(size).flatMap(f))
  }
}

object SGen {
  def unit[A](a: A): SGen[A] = {
    Gen(State.unit(a)).unsized
  }

  def map2[A,B,C](a: SGen[A], b: SGen[B])(f: (A,B) => C): SGen[C] = {
    SGen { size =>
      Gen.map2(a.forSize(size), b.forSize(size))(f)
    }
  }

  def sequence[A](fs: List[SGen[A]]): SGen[List[A]] = {
    SGen[List[A]] { size =>
      Gen.sequence(fs.map(_.forSize(size)))
    }
  }

  def weighted[A](g1: (SGen[A],Double), g2: (SGen[A],Double)): SGen[A] = {
    SGen[A] { size =>
      Gen.weighted(
        (g1._1.forSize(size), g1._2),
        (g2._1.forSize(size), g2._2))
    }
  }

  def listOf[A](a: Gen[A]): SGen[List[A]] = {
    SGen { size => Gen.listOfN(size, a) }
  }

  def listOf1[A](a: Gen[A]): SGen[List[A]] = {
    SGen { size => Gen.listOfN(size max 1, a) }
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

    ps("sentence", Gen.sentence)

    ps("union", Gen.union(Gen.choose(0,10), Gen.choose(990, 1000)))
    ps("weighted", Gen.weighted(
      (Gen.choose(0,10), 0.2),
      (Gen.choose(990, 1000), 0.8)))

    ps("listOfN random size",
      Gen.listOfN(Gen.choose(0, 10), Gen.boolean))

    println("test runs ------------------")
    val rng = SimpleRNG(101)

    println(
      Prop.forAll(Gen.choose(0, 10))(_ < 10).run(-1, 20, rng))

    println(
      Prop.forAll(Gen.choose(0, 10))(_ > 3).run(-1, 20, rng))

    println("sized runs -------------------")
    println(
      Prop.forAll(SGen.listOf(Gen.choose(-10, 10))) { _.length < 5 }.run(10, 200, rng)
    )

    run("max") {
      Prop.forAll(SGen.listOf(Gen.choose(-10, 10))) { list =>
        list.forall { _ <= list.max }
      }
    }

    run("empty list") {
      Prop.forAll(SGen.listOf(Gen.choose(-10, 10))) {
        _.nonEmpty
      }
    }

    println("sort ----------------------------------")

    run("sort spec") {
      val list = SGen.listOf(Gen.choose(-10, 10))
      val list1 = SGen.listOf1(Gen.choose(-10, 10))

      Prop.forAll(list) { l => l.sorted.sorted == l.sorted } &&
      Prop.forAll(list1) { l => l.sorted.head == l.min } &&
      Prop.forAll(list1) { l => l.sorted.last == l.max } &&
      Prop.forAll(list1) { l =>
        val s = l.sorted
        s.zip(s.drop(1)).forall { case(a, b) => a <= b }
      }
    }
  }

  private def run(name: String)(p: Prop): Unit = {
    val rng = SimpleRNG(101)

    print(s"$name: ")
    println(p.run(10, 200, rng))
  }

  private def ps[A](name: String, g: Gen[A]): Unit = {
    val rng: RNG = SimpleRNG(1)
    print(s"$name: ")
    val genList = Gen.listOfN(10, g).sample.run(rng)._1
    println(s"$genList")
  }
}
