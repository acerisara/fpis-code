package fpis.code.chapter6

import fpis.code.chapter6.SimpleRNG.nonNegativeInt

object Rand {

  type Rand[+A] = State[RNG, A]

  val int: Rand[Int] = State(_.nextInt)

  def unit[A](a: A): Rand[A] = State.unit(a)

  def map[A, B](ra: Rand[A])(f: A => B): Rand[B] = ra.map(f)

  def flatMap[A, B](ra: Rand[A])(f: A => Rand[B]): Rand[B] = ra.flatMap(f)

  def nonNegativeEven: Rand[Int] =
    map(State(nonNegativeInt))(i => i - i % 2)

  def double: Rand[Double] =
    map(State(nonNegativeInt))(i => i / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    ra.map2(rb)(f)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  def ints(count: Int): Rand[List[Int]] = State.sequence(List.fill(count)(int))

}
