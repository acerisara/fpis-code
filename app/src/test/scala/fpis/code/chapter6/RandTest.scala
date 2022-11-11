package fpis.code.chapter6

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RandTest extends AnyFunSuite {

  val seed = 123456789
  val rng: SimpleRNG = SimpleRNG(seed)

  test("Rand.ints") {
    val is = Rand.ints(5).run(rng)._1

    is should be(
      List(1820451251, 1221384887, 1220957452, 2086077588, -284667191)
    )
  }

}
