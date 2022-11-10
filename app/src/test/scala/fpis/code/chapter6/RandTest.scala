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
    val is1 = Rand.ints(5)(rng)._1
    val is2 = Rand.intsU(5)(rng)._1

    is1 should be(
      List(-284667191, 2086077588, 1220957452, 1221384887, 1820451251)
    )

    // TODO: Check ordering here
//    is1 should be(is2)
  }

}
