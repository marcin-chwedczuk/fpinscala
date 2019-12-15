package pl.marcinchwedczuk.fpinscala.chp9

import pl.marcinchwedczuk.fpinscala.chp6.SimpleRNG
import pl.marcinchwedczuk.fpinscala.chp8.{Gen, Prop}

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))
  def string(s: String): Parser[String]
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n <= 0) succeed(List())
    else (p ~> listOfN(n-1, p)).map { case(p, ps) => p :: ps }

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def input[A](p: Parser[A]): Parser[String]

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) | succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    (p ~> many(p)).map { case (a, as) => a :: as }

  def concat[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  def or[A](l: Parser[A], r: => Parser[A]): Parser[A]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B]
  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A,B) => C): Parser[C] =
    (pa ~> pb).map { case (a, b) => f(a, b) }
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def stringToParser(s: String): Parser[String]
  implicit def parserToRichParser[A](p: Parser[A]): RichParser[A] =
    RichParser(p)
  implicit def valueToParserImplicitly[A](a: A)
                                         (implicit f: A => Parser[String]): RichParser[String] =
    RichParser(f(a))
  implicit def regex(r: Regex): Parser[String]


  case class RichParser[A](p: Parser[A]) {
    def many: Parser[List[A]] = self.many(p)
    def oneOrMore: Parser[List[A]] = self.many1(p)
    def repeat(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def ~>[B](p2: => Parser[B]): Parser[(A,B)] = self.concat(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def input: Parser[String] = self.input(p)
  }
}

object ParsersTesting {
  def main(args: Array[String]): Unit = {

    println("ok")
  }

  def testImpl[ParseError, Parser[+_]](t: Parsers[ParseError, Parser]): Unit = {
    import t._

    check("char('a')") {
      Prop.forAll(Gen.lowerCaseLetter) { c =>
        run(char(c))(c.toString) == Right(c)
      }
    }

    check("string('foo')") {
      Prop.forAll(Gen.word(1, 10)) { w =>
        run(string(w))(w) == Right(w)
      }
    }

    check("or combinator") {
      Prop.assert {
        val p = or(string("foo"), string("bar"))

        run(p)("foo") == Right("foo")
        run(p)("bar") == Right("bar")
      }
    }

    check("listOfN") {
      Prop.assert {
        run(listOfN(3, "foo" | "bar"))("foofoofoo") == Right("foofoofoo") &&
        run(listOfN(3, "foo" | "bar"))("foobarfoo") == Right("foobarfoo") &&
        run(listOfN(3, "foo" | "bar"))("barbarfoo") == Right("barbarfoo")
      }
    }

    def equal[A](l: Parser[A], r: Parser[A])(in: Gen[String]): Prop = {
      Prop.forAll(in) { s => run(l)(s) == run(r)(s) }
    }

    check("map law") {
      equal(string("foo"), string("foo").map(x => x))(Gen.sentence)
    }

    check("succeed") {
      Prop.assert {
        run(succeed(1))("") == Right(1)
      }
    }

    check("4aaaa parser") {
      val p = regex("[0-9]+".r)
        .map(_.toInt)
        .flatMap(n => char('a').repeat(n))

      Prop.assert { run(p)("4aaaa") == Right("aaaa") }
      Prop.assert { run(p)("4aaa").isInstanceOf[Left] }
    }
  }

  private def check(name: String)(p: Prop): Unit = {
    val rng = SimpleRNG(101)

    print(s"$name: ")
    println(p.run(10, 200, rng))
  }
}
