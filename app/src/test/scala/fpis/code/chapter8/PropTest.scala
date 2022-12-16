package fpis.code.chapter8

import fpis.code.chapter6.{RNG, SimpleRNG}
import fpis.code.chapter7.Par
import fpis.code.chapter8.Prop.{MaxSize, TestCases, forAll, forAllPar}
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

    run(maxProp) should be(Passed)
  }

  test("List.sorted property") {
    val smallInt = Gen.choose(-10, 10)

    val sortedProp = forAll(SGen.listOf(smallInt)) { ns =>
      val sorted = ns.sorted
      sorted.sliding(2).forall {
        case Nil      => true
        case _ :: Nil => true
        case x :: xs  => x <= xs.head
      }
    }

    run(sortedProp) should be(Passed)
  }

  test("Par.map law") {
    val ints = Gen.choose(0, 100)

    val mapProp = forAllPar(ints) { i =>
      Par.equal(
        Par.map(Par.unit(i))(_ + 1),
        Par.unit(i + 1)
      )
    }

    run(mapProp) should be(Passed)
  }

  test("Par.map identity law") {
    val pints = Gen.choose(0, 10).map(Par.unit)

    val mapProp = forAllPar(pints) { i =>
      Par.equal(
        Par.map(i)(x => x),
        i
      )
    }

    run(mapProp) should be(Passed)
  }

  private def run(
      p: Prop,
      maxSize: MaxSize = 100,
      testCases: TestCases = 100,
      rng: RNG = SimpleRNG(System.currentTimeMillis())
  ): Result = p.run(maxSize, testCases, rng)

}
