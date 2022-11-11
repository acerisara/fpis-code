package fpis.code.chapter6

import fpis.code.chapter6.SimpleRNG.nonNegativeInt

object Rand {

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def flatMap[A, B](s: Rand[A])(f: A => Rand[B]): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      f(a)(rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] =
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def map2F[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a =>
      map(rb) { b =>
        f(a, b)
      }
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def mapF[A, B](ra: Rand[A])(f: A => B): Rand[B] =
    flatMap(ra) { a =>
      unit(f(a))
    }

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    rng => {
      ras.foldRight((List.empty[A], rng))((ra, b) => {
        // This is essentially map2
        val (as, rng) = b
        val (a, rng2) = ra(rng)
        (as :+ a, rng2)
      })
    }

  def sequenceU[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight(unit(List.empty[A]))((ra, ras) => map2(ra, ras)(_ :: _))

  def ints(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def intsU(count: Int): Rand[List[Int]] = sequenceU(List.fill(count)(int))

}
