package fpis.code.chapter11

import fpis.code.chapter11.Monad.{optionMonad, stateMonad, zipWithIndex}
import fpis.code.chapter6.{RNG, Rand, SimpleRNG}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MonadTest extends AnyFunSuite {

  val om: Monad[Option] = optionMonad

  val seed = 123456789
  val rng: SimpleRNG = SimpleRNG(seed)

  test("Monad.sequence") {
    om.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
    om.sequence(List(Some(1), None, Some(3))) should be(None)
  }

  test("Monad.traverse") {
    val f = (i: Int) => if (i < 5) Some(i) else None

    om.traverse(List(1, 2, 3))(f) should be(Some(List(1, 2, 3)))
    om.traverse(List(1, 6, 2))(f) should be(None)
  }

  test("Monad.replicatedM") {
    om.replicateM(3, Some(1)) should be(Some(List(1, 1, 1)))
    om.replicateM(3, None) should be(None)
  }

  test("Monad.product") {
    om.product(Some(1), Some("a")) should be(Some((1, "a")))
    om.product(Some(1), None) should be(None)
  }

  test("Monad.filterM") {
    val f = (i: Int) => if (i < 5) Some(true) else Some(false)
    val n = (i: Int) => if (i < 5) Some(true) else None

    om.filterM(List(1, 2, 3))(f) should be(Some(List(1, 2, 3)))
    om.filterM(List(1, 6, 3))(f) should be(Some(List(1, 3)))
    om.filterM(List(1, 6, 3))(n) should be(None)
  }

  test("Monad.compose") {
    val f = (i: Int) => if (i > 0) Some(i.toString) else Some("")
    val g = (s: String) => if (s.nonEmpty) Some(true) else Some(false)

    val h = om.compose(f, g)

    h(1) should be(Some(true))
    h(0) should be(Some(false))
  }

  test("Id monad") {
    // The behaviour of the id monad is just variable substitution
    val m = for {
      a <- Id("Hello, ")
      b <- Id("monad!")
    } yield a + b

    m should be(Id("Hello, monad!"))
  }

  test("State monad replicateM") {
    // replicateM runs the state n times and returns
    // the list of produced values plus the final state
    val m = stateMonad[RNG]

    val a1 = Rand.int.run(rng)
    val a2 = Rand.int.run(a1._2)
    val a3 = Rand.int.run(a2._2)

    val (ints, finalState) = m.replicateM(3, Rand.int).run(rng)

    ints should be(List(a1._1, a2._1, a3._1))
    finalState should be(a3._2)
  }

  test("State monad map2") {
    // map2 runs the two states and returns
    // the combined value plus the state of the second invocation
    val m = stateMonad[RNG]

    val a1 = Rand.int.run(rng)
    val a2 = Rand.int.run(a1._2)

    val (ints, finalState) = m.map2(Rand.int, Rand.int)(_ + _).run(rng)

    ints should be(a1._1 + a2._1)
    finalState should be(a2._2)
  }

  test("State monad sequence") {
    // sequence runs each state in the list
    // and returns the list of produced values plus the final state
    val m = stateMonad[RNG]

    val a1 = Rand.int.run(rng)
    val a2 = Rand.int.run(a1._2)
    val a3 = Rand.int.run(a2._2)

    val (ints, finalState) =
      m.sequence(List(Rand.int, Rand.int, Rand.int)).run(rng)

    ints should be(List(a1._1, a2._1, a3._1))
    finalState should be(a3._2)
  }

  test("Zip with index") {
    zipWithIndex(List("a", "b", "c")) should be(
      List(
        (0, "a"),
        (1, "b"),
        (2, "c")
      )
    )
  }

}
