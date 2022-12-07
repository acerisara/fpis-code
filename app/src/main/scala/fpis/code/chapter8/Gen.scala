package fpis.code.chapter8

import fpis.code.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

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
    Gen(
      State(SimpleRNG.nonNegativeInt).map(n => n % 2 == 0)
    )

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(
      State.sequence(List.fill(n)(g.sample))
    )

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
