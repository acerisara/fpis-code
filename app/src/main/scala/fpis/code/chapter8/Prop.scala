package fpis.code.chapter8

import fpis.code.chapter6.RNG
import fpis.code.chapter8.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case Passed => p.run(m, n, rng)
      case x      => x
    }
  }

  def ||(p: Prop): Prop = Prop { (m, n, rng) =>
    run(m, n, rng) match {
      case Falsified(_, _) => p.run(m, n, rng)
      case x               => x
    }
  }

}

sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount)
    extends Result {
  def isFalsified = true
}

object Prop {

  type TestCases = Int
  type FailedCase = String
  type SuccessCount = Int
  type MaxSize = Int

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop { (_, n, rng) =>
    randomStream(as)(rng)
      .zip(LazyList.from(0))
      .take(n)
      .map { case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
      }
      .find(_.isFalsified)
      .getOrElse(Passed)
  }

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop = forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: LazyList[Prop] =
        LazyList.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))

      val prop: Prop =
        props
          .map(p =>
            Prop { (max, _, rng) =>
              p.run(max, casesPerSize, rng)
            }
          )
          .toList
          .reduce(_ && _)

      prop.run(max, n, rng)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): LazyList[A] =
    LazyList.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

}
