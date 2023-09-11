package fpis.code.chapter8

import fpis.code.chapter6.{RNG, SimpleRNG, State}

case class Gen[+A](sample: State[RNG, A]) {

  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => this.listOfN(n))

  def listOfN(size: Int): Gen[List[A]] =
    Gen.listOfN(size, this)

  def **[B](g: Gen[B]): Gen[(A, B)] = map2(g)((_, _))

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(
      State(SimpleRNG.nonNegativeInt).map(n =>
        start + n % (stopExclusive - start)
      )
    )

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def boolean: Gen[Boolean] =
    Gen(State(SimpleRNG.nonNegativeInt).map(n => n % 2 == 0))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    List.fill(n)(g).foldRight(unit(List[A]()))((a, as) => a.map2(as)(_ :: _))

  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(if (_) g1 else g2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val threshold = g1._2.abs / (g1._2.abs + g2._2.abs)

    Gen(
      State(SimpleRNG.double).flatMap(d =>
        if (d < threshold) g1._1.sample else g2._1.sample
      )
    )
  }

}
