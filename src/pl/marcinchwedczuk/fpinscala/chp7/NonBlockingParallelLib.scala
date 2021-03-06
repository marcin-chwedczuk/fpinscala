package pl.marcinchwedczuk.fpinscala.chp7

import java.util.concurrent._
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference}

import scala.util.{Failure, Success, Try}

object NonBlockingParallelLib {
  trait Future[+A] {
    private[chp7] def apply(callback: Try[A] => Unit): Unit
  }

  type Par[+A] = ExecutorService => Future[A]

  // To avoid StackOverflowExceptions we need to move
  // all callbacks to a separate tasks.

  object Par {
    def unit[A](a: => A): Par[A] = {
      _ => new Future[A] {
        override private[chp7] def apply(callback: Try[A] => Unit): Unit =
          callback(Try(a))
      }
    }

    private def schedule(es: ExecutorService)(work: => Unit) = {
      es.submit(new Callable[Unit] {
        override def call(): Unit = {
          try {
            work
          }
          catch {
            case e: Throwable =>
              println(s"INTERNAL ERROR: $e")
              e.printStackTrace()
              throw e
          }
        }
      })
    }

    def fork[A](a: => Par[A]): Par[A] = {
      es => new Future[A] {
        override private[chp7] def apply(callback: Try[A] => Unit): Unit = {
          schedule(es) {
            a(es)(callback(_))
          }
        }
      }
    }

    def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A,B) => C): Par[C] = {
      es => new Future[C] {
        private val resultsNumber = new AtomicInteger(0)
        private val va = new AtomicReference[Try[A]]()
        private val vb = new AtomicReference[Try[B]]()

        override private[chp7] def apply(callback: Try[C] => Unit): Unit = {
          def runIfReady(): Unit = {
            if (resultsNumber.incrementAndGet() == 2) {
              schedule(es) {
                callback(
                  for {
                    aa <- va.get()
                    bb <- vb.get()
                  } yield f(aa, bb)
                )
              }
            }
          }

          pa(es) { aa =>
            va.set(aa)
            runIfReady()
          }

          pb(es) { bb =>
            vb.set(bb)
            runIfReady()
          }
        }
      }
    }

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def parMap[A,B](as: List[A])(f: A => B): Par[List[B]] = fork {
      // F**ing List[A] is not 100% f**ing thread safe:
      // https://github.com/scala/bug/issues/7838
      type Q = ConcurrentLinkedQueue[B]

      as.foldLeft(unit(new Q())) { (list: Par[Q], a: A) =>
        // Need to fork to avoid StackOverflow when evaluating list Par
        fork(map2(lazyUnit(f(a)), list) { (aa, q) =>
          q.offer(aa)
          q
        })
      }.map(q => q.toArray().toList.asInstanceOf[List[B]])
    }

    def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = fork {
      sequence {
        as.map(a => lazyUnit[Option[A]](Some(a).filter(f)))
      }
      .map(_.filterNot(_.isEmpty).map(_.get))
    }

    def sequence[A](pas: List[Par[A]]): Par[List[A]] = fork {
      pas.foldRight(unit(List[A]())) { (pa, plist) =>
        map2(pa, plist)(_ :: _)
      }
    }

    def asyncF[A,B](f: A => B): A => Par[B] = {
      a => lazyUnit(f(a))
    }

    def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
      n.flatMap { nn =>
        choices(nn)
      }
    }

    def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
      choiceN(cond.map(t => if (t) 0 else 1))(List(t, f))
    }
  }

  case class RichPar[+A](self: Par[A]) {
    def run(es: ExecutorService): Try[A] = {
      val ref = new AtomicReference[Try[A]]()
      val latch = new CountDownLatch(1)

      self(es) { a =>
        ref.set(a)
        latch.countDown()
      }

      latch.await()
      ref.get()
    }

    def map[B](f: A => B): Par[B] = {
      Par.map2(self, Par.unit(())) { (av, _) => f(av) }
    }

    def flatMap[B](f: A => Par[B]): Par[B] = {
      es => new Future[B] {
        override private[chp7] def apply(callback: Try[B] => Unit): Unit = {
          self(es) { tryA =>
            tryA.map(f) match {
              case Success(bb) => bb(es).apply(callback)
              case Failure(exception) => callback(Failure[B](exception))
            }
          }
        }
      }
    }
  }

  implicit def parToRichPar[A](p: Par[A]): RichPar[A] =
    new RichPar[A](p)

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(8)

    val left = Par.fork(Par.unit[Int] { wait1s(); 1 })
    val right = Par.fork(Par.unit[Int] { wait1s(); 10 })

    withTime {
      val ans = Par.map2(left, right) { (a, b) =>
        a + b
      }.run(es)

      println(s"ans = $ans")
    }

    /*
    withTime {
      println("sequence")
      val ai = new AtomicInteger(0)
      val ci = new AtomicInteger(0)
      for (i <- 1 to 5000) {
        es.submit(new Runnable {
          override def run(): Unit =  {
            Par.map2(
              Par.lazyUnit { ai.incrementAndGet();  },
              Par.lazyUnit { ai.incrementAndGet(); Thread.sleep(1) })((_,_) => ())(es) { _ =>
              ci.incrementAndGet()
            }
          }
        })
      }

      es.awaitTermination(10, TimeUnit.SECONDS)
      println("ai = " + ai.get())
      println("ci = " + ci.get())
    }
    */

    withTime {
      println("before parMap")
      val res = Par.parMap(List.range(1, 100000))(math.sqrt(_)).run(es)
      println("after")
      println("parMap = " + res.get.take(100))
    }

    println("Error handling with Try:")
    println(
      Par.unit(0).map(z => 1 / z).map(_.toString).run(es)
    )

    println(
      Par.lazyUnit[Int](throw new RuntimeException("foo"))
        .map(z => 1 / z).map(_.toString).run(es)
    )

    println("choiceN:")
    println(
      Par.choiceN(Par.lazyUnit(-3))(List(
        Par.lazyUnit("0"),
        Par.lazyUnit("1"),
        Par.lazyUnit("2"))).run(es))

    println(
      Par.choiceN(Par.lazyUnit(1))(List(
        Par.lazyUnit("0"),
        Par.lazyUnit("1foo"),
        Par.lazyUnit("2"))).run(es))
    es.shutdown()
  }

  private def withTime(f: => Unit): Unit = {
    val start = System.currentTimeMillis()
    try {
      f
    }
    finally {
      val stop = System.currentTimeMillis()
      println("took (ms): " + (stop - start))
    }
  }

  def wait1s(): Unit = {
    Thread.sleep(1000)
  }
}
