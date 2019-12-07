package pl.marcinchwedczuk.fpinscala.chp7

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent._

object ParallelLib {
  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def isCancelled: Boolean = false
    override def get(timeout: Long, unit: TimeUnit): A = get
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false
  }

  private case class Map2Future[A,B,C](a: Future[A],
                                       b: Future[B],
                                       f: (A,B) => C)
    extends Future[C]
  {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = {
      if (!a.isCancelled && !a.cancel(mayInterruptIfRunning)) return false
      if (!b.isCancelled && !b.cancel(mayInterruptIfRunning)) return false
      true
    }

    override def isCancelled: Boolean = a.isCancelled && b.isCancelled
    override def isDone: Boolean = a.isDone && b.isDone

    //                          more than 1 billion years...
    override def get(): C = get(Long.MaxValue, TimeUnit.NANOSECONDS)

    private val r: AtomicReference[Option[C]] = new AtomicReference[Option[C]](None)

    override def get(timeout: Long, unit: TimeUnit): C = {
      r.get().getOrElse {
        val timeoutMilis = unit.toMillis(timeout)
        val startMillis = System.currentTimeMillis()
        val aValue = a.get(timeoutMilis, TimeUnit.MILLISECONDS)
        val endMillis = System.currentTimeMillis()
        val usedTimeoutMillis = endMillis - startMillis
        val bValue = b.get(timeoutMilis - usedTimeoutMillis, TimeUnit.MILLISECONDS)
        val result = f(aValue, bValue)
        r.set(Some(result))
        result
      }
    }
  }

  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] =
    _ => UnitFuture(a)

  def fork[A](a: => Par[A]): Par[A] = { es =>
    es.submit(new Callable[A] {
      override def call(): A = a(es).get()
    })
  }

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] =
    a(s)

  // combinators:
  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = { es =>
    val futureA = a(es)
    val futureB = b(es)

    Map2Future(futureA, futureB, f)
  }

  // derived:
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] = {
    a => lazyUnit(f(a))
  }

  def sum(ints: IndexedSeq[Int]): Par[Int] = {
    if (ints.length <= 1)
     unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      lazy val sumL: Par[Int] = fork(sum(l))
      lazy val sumR: Par[Int] = fork(sum(r))
      map2(sumL, sumR)(_ + _)
    }
  }

  def main(args: Array[String]): Unit = {
    val es = Executors.newFixedThreadPool(10)

    val ints = (1 to 8)
        // .map(_ => ThreadLocalRandom.current().nextInt())
        .toIndexedSeq

    val ans = sum(ints)
    println("result = " + run(es)(ans).get())

    val wait: Int => Int = n => { Thread.sleep(1000); n }
    val asyncWait: Int => Par[Int] = asyncF(wait)

    val tmp1 = map2(
      map2(asyncWait(3), asyncWait(4))(_ + _),
      map2(asyncWait(1), asyncWait(2))(_ + _))(_ + _)

    val start = System.currentTimeMillis()
    println("sum = " + run(es)(tmp1).get())
    val stop = System.currentTimeMillis()
    println("took (ms): " + (stop - start))

    es.shutdown()
  }
}
