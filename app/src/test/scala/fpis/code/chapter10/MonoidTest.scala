package fpis.code.chapter10

import fpis.code.chapter10.Monoid.{intAdditionMonoid, monoidLaws}
import fpis.code.chapter6.{RNG, SimpleRNG}
import fpis.code.chapter8.{Gen, Passed, Prop, Result}
import fpis.code.chapter8.Prop.{MaxSize, TestCases}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

@RunWith(classOf[JUnitRunner])
class MonoidTest extends AnyFunSuite {

  test("Int addition monoid") {
    val gen = Gen.choose(0, 100)
    val p = monoidLaws(intAdditionMonoid, gen)

    run(p) should be(Passed)
  }

  private def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis())
  ): Result = p.run(maxSize, testCases, rng)

}
