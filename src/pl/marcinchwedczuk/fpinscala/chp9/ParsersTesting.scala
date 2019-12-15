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


  def toValue[A,B](a: Parser[A])(b: => B): Parser[B] =
    map(a)(_ => b)

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def input[A](p: Parser[A]): Parser[String]

  def zeroOrOne[A](p: Parser[A]): Parser[Option[A]] = {
    p.map(Some(_)) | succeed(None)
  }

  def many[A](p: Parser[A]): Parser[List[A]] =
    many1(p) | succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    (p ~> many(p)).map { case (a, as) => a :: as }

  def concat[A,B](p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    p1.flatMap { a => p2.map(b => (a, b)) }
  def or[A](l: Parser[A], r: => Parser[A]): Parser[A]

  def map[A,B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))
  def map2[A,B,C](pa: Parser[A], pb: => Parser[B])(f: (A,B) => C): Parser[C] =
    pa.flatMap { a => pb.map(b => f(a, b)) }
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
    def zeroOrOne: Parser[Option[A]] = self.zeroOrOne(p)
    def oneOrMore: Parser[List[A]] = self.many1(p)
    def repeat(n: Int): Parser[List[A]] = self.listOfN(n, p)

    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
    def ~>[B](p2: => Parser[B]): Parser[(A,B)] = self.concat(p, p2)

    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)

    def to[B](b: => B): Parser[B] = self.toValue(p)(b)
    def input: Parser[String] = self.input(p)
  }
}

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: List[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON
}

object ParsersTesting {
  def main(args: Array[String]): Unit = {

    println("ok")
  }

  def jsonParser[Err, Parser[+_]](P: Parsers[Err, Parser]): Parser[JSON] = {
    import P._
    import JSON._

    // no spaces at beginning and end
    var json: Parser[JSON] = succeed(JNull)

    val jnumber = regex("[0-9](.[0-9]+)?([eE][0-9]+)?".r)
        .map(_.toDouble)
        .map(JNumber)

    val hexDigit = regex("[0-9a-fA-F]".r)

    val nonEscapedChar = regex("[^\\\\]".r)
    val escapedChar = (char('\\') ~> (
      char('n').to("\n") |
      char('t').to("\t") |
      char('r').to("\r") |
      char('\\').to("\\") |
      char('"').to("\"") |
      char('\'').to("'") |
      (char('u') ~> hexDigit.repeat(4)).map { case (_, hexDigits) =>
        val charCode = Integer.parseInt(hexDigits.mkString(""), 16)
        charCode.toChar.toString
      }
    )).map { case(_, s) => s }

    val jstring = (char('"') ~> (nonEscapedChar | escapedChar).many ~> char('"'))
        .map { case ((_, b), _) => "\"" + b + "\"" }
        .map(JString)

    val jnull = string("null").to(JNull)

    val jbool = string("true").to(JBool(true)) |
                string("false").to(JBool(false))

    val ws = regex("[ \t\n\r]*".r).to(())

    val jarrayElement = (ws ~> json).map { case (_, j) => j }
    val jarrayNextElement = (ws ~> char(',') ~> ws ~> json)
      .map { case (_, j) => j }

    val jarrayElements = (jarrayElement ~> jarrayNextElement.many)
      .map { case (el, list) => el :: list }
      .zeroOrOne
      .map { opt => opt.getOrElse(List()) }

    val jarray = (char('[') ~> jarrayElements ~> ws ~> char(']'))
        .map { case (((_, l), _), _) => l }
        .map(JArray)


    val keyValue = (jstring ~> (ws ~> char(':') ~> ws) ~> json)
      .map { case ((key, _), obj) => key -> obj }

    val keyValueNext = ((ws ~> char(',') ~> ws) ~> jstring ~> (ws ~> char(':') ~> ws) ~> json)
      .map { case (((_, key), _), obj) => (key -> obj) }

    val keyValues = (keyValue ~> keyValueNext.many)
      .map { case (p, pairs) => p :: pairs }
      .zeroOrOne
      .map { opt => opt.getOrElse(List()) }

    // we don't check for duplicated keys
    val jobject = (char('{') ~> keyValues ~> (ws ~> char('}')))
        .map { case ((_, pairs), _) => pairs.map(p => (p._1.get, p._2)).toMap }
        .map(JObject)

    json = jnull | jbool | jnumber | jstring | jarray | jobject

    json
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
      Prop.assert { run(p)("4aaa").isInstanceOf[Left[_,_]] }
    }
  }

  private def check(name: String)(p: Prop): Unit = {
    val rng = SimpleRNG(101)

    print(s"$name: ")
    println(p.run(10, 200, rng))
  }
}
