package fpis.code.chapter8

import fpis.code.chapter6.SimpleRNG
import fpis.code.chapter8.Prop.forAll
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class PropTest extends AnyFunSuite {

  test("List.max property") {
    val smallInt = Gen.choose(-10, 10)

    val maxProp = forAll(SGen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }

    val result = maxProp.run(100, 100, SimpleRNG(System.currentTimeMillis()))
    result should be(Passed)
  }

}
