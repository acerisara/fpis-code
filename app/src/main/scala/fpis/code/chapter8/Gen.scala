package fpis.code.chapter8

import fpis.code.chapter6.{RNG, SimpleRNG, State}

case class Gen[A](sample: State[RNG, A])

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

}
