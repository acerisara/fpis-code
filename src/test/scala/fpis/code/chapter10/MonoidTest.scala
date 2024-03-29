package fpis.code.chapter10

import fpis.code.chapter10.Monoid._
import fpis.code.chapter6.{RNG, SimpleRNG}
import fpis.code.chapter8.Prop.{MaxSize, TestCases}
import fpis.code.chapter8.{Gen, Passed, Prop, Result}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import java.util.concurrent.{ExecutorService, Executors}

@RunWith(classOf[JUnitRunner])
class MonoidTest extends AnyFunSuite {

  val es: ExecutorService = Executors.newFixedThreadPool(5)

  test("Int addition monoid") {
    val gen = Gen.choose(0, 100)
    val p = monoidLaws(intAdditionMonoid, gen)

    run(p) should be(Passed)
  }

  test("Monoid.concatenate") {
    val ints = List(1, 2, 3, 4, 5)

    concatenate(ints, intAdditionMonoid) should be(15)
  }

  test("Monoid.foldMap (and variants)") {
    val s = List("1", "2", "3", "4", "5")

    foldMap(s, intAdditionMonoid)(_.toInt) should be(15)
    foldMapV(s.toIndexedSeq, intAdditionMonoid)(_.toInt) should be(15)

    val p = parFoldMap(s.toIndexedSeq, intAdditionMonoid)(_.toInt)
    p(es).get should be(15)
  }

  test("Monoid.isOrdered") {
    isOrdered(IndexedSeq(1, 2, 3, 4, 5)) should be(true)
    isOrdered(IndexedSeq(1, 1, 3, 4, 5)) should be(true)
    isOrdered(IndexedSeq(5, 5, 5, 5, 5)) should be(true)
    isOrdered(IndexedSeq(1, 2, 3, 5, 4)) should be(false)
    isOrdered(IndexedSeq(2, 1, 3, 4, 5)) should be(false)
    isOrdered(IndexedSeq(1, 2, 5, 4, 3)) should be(false)
    isOrdered(IndexedSeq(5, 4, 3, 2, 1)) should be(false)
  }

  test("Product monoid") {
    val gen = Gen.choose(0, 100)

    val p = monoidLaws(
      productMonoid(intAdditionMonoid, intMultiplicationMonoid),
      gen ** gen
    )

    run(p) should be(Passed)
  }

  test("Monoid.bag") {
    val expected = Map(
      "a" -> 2,
      "is" -> 1,
      "rose" -> 2
    )

    bag(Vector("a", "rose", "is", "a", "rose")) should be(expected)
    bagM(Vector("a", "rose", "is", "a", "rose")) should be(expected)
  }

  private def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis())
  ): Result = p.run(maxSize, testCases, rng)

}
