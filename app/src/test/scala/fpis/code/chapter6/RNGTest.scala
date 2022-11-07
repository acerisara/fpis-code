package fpis.code.chapter6

import fpis.code.chapter6.SimpleRNG._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{be, not}
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RNGTest extends AnyFunSuite {

  val seed = 123456789
  val rng: SimpleRNG = SimpleRNG(seed)

  test("SimpleRNG.nonNegativeInt") {
    // Same input state produces the same result
    nonNegativeInt(rng)._1 should be(1820451251)
    nonNegativeInt(rng)._1 should be(1820451251)
    nonNegativeInt(rng)._1 should be(1820451251)

    val rng2 = nonNegativeInt(rng)._2

    nonNegativeInt(rng2)._1 should not(be(1820451251))
  }

  test("SimpleRNG.double") {
    double(rng)._1 should be(0.8477136730216444)
  }

  test("SimpleRNG.intDouble") {
    val (i, d) = intDouble(rng)._1
    i should be(1820451251)
    d should be(0.5687516587786376)
  }

  test("SimpleRNG.doubleInt") {
    val (d, i) = doubleInt(rng)._1
    d should be(0.5687516587786376)
    i should be(1820451251)
  }

  test("SimpleRNG.double3") {
    val (d1, d2, d3) = double3(rng)._1
    d1 should be(0.8477136730216444)
    d2 should be(0.5687516587786376)
    d3 should be(0.5685526188462973)
  }

  test("SimpleRNG.ints") {
    val is = ints(5)(rng)._1

    is should be(
      List(-284667191, 2086077588, 1220957452, 1221384887, 1820451251)
    )
  }

}
