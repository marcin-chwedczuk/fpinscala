package pl.marcinchwedczuk.fpinscala.chp4

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = {
    this match {
      case Right(v) => Right(f(v))
      case l: Left[E] => l
    }
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = {
    this match {
      case Right(v) => f(v)
      case l: Left[E] => l
    }
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = {
    this match {
      case r: Right[A] => r
      case _: Left[E] => b
    }
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this.flatMap { av =>
      b.map { bv =>
        f(av, bv)
      }
    }
  }
}

case class Left[E](value: E) extends Either[E, Nothing]
case class Right[A](value: A) extends Either[Nothing, A]

object Either {
  def Try[A](f: => A): Either[Exception, A] = {
    try {
      Right(f)
    } catch {
      case e: Exception => Left(e)
    }
  }

  def traverse[E,A,B](as: List[A])(f: A => Either[E,B]): Either[E, List[B]] = {
    as.foldRight(Right(List()): Either[E, List[B]]) { (a, rl) =>
      rl.flatMap { l =>
        f(a).map(fa => fa :: l)
      }
    }
  }
}

object EitherProgram {
  def main(args: Array[String]): Unit = {
    import Either._

    val r1: Either[String, Int] = Right(1)
    val r2: Either[String, Int] = Right(2)
    val lErr: Either[String, Int] = Left("err")

    println {
      traverse(List("12", "15", "17"))(s => Try(s.toInt))
    }

    println {
      traverse(List("12", "foo", "17"))(s => Try(s.toInt))
    }
  }
}
