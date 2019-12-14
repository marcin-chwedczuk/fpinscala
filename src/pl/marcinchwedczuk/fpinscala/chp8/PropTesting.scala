package pl.marcinchwedczuk.fpinscala.chp8

trait Gen[+A] {

}

trait Prop { self =>
  def check(): Boolean

  def &&(other: Prop): Prop = {
    new Prop {
      override def check(): Boolean =  {
        self.check() && other.check()
      }
    }
  }
}

object Gen {
  def listOf[A](element: Gen[A]): Gen[List[A]] = {
    ???
  }

  def forAll[A](a: Gen[A])(p: A => Boolean): Prop = {
    ???
  }
}

object PropTesting {

  def main(args: Array[String]): Unit = {
    println("ok")
  }
}
