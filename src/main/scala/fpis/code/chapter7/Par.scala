package fpis.code.chapter7

import java.util.concurrent.{Callable, ExecutorService, Future, TimeUnit}

object Par {

  type Par[A] = ExecutorService => Future[A]

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit): A = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def map2[A, B, C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] =
    es => {
      val af = pa(es)
      val bf = pb(es)
      UnitFuture(f(af.get, bf.get))
    }

  def fork[A](pa: => Par[A]): Par[A] =
    es => {
      es.submit(new Callable[A] {
        def call: A = pa(es).get
      })
    }

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](es: ExecutorService)(pa: Par[A]): Future[A] = pa(es)

  def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def parMap[A, B](as: List[A])(f: A => B): Par[List[B]] = fork {
    sequence(as.map(asyncF(f)))
  }

  def sequence[A](pas: List[Par[A]]): Par[List[A]] =
    pas.foldRight(unit(List.empty[A]))((pa, pas) => map2(pa, pas)(_ :: _))

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val pbs = sequence(as.map(asyncF(f)))
    map(pbs)(_.zip(as).filter(_._1).map(_._2))
  }

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    chooser(cond)(if (_) t else f)

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] =
    chooser(n)(choices(_))

  // `chooser` is generally called `flatMap`
  def chooser[A, B](pa: Par[A])(choices: A => Par[B]): Par[B] =
    es => {
      val a = run(es)(pa).get()
      run(es)(choices(a))
    }

  def join[A](a: Par[Par[A]]): Par[A] =
    es => {
      val pa = run(es)(a).get()
      run(es)(pa)
    }

  def flatMap[A, B](a: Par[A])(f: A => Par[B]): Par[B] = join(map(a)(f))

  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] = Par.map2(p, p2)(_ == _)

}
