package fpis.code.chapter15

import fpis.code.chapter15.Process.{filter, liftOne, sum}
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ProcessTest extends AnyFunSuite {

  test("Process.liftOne") {
    val double = (i: Int) => i * 2
    liftOne(double)(LazyList(1, 2, 3)).toList should be(List(2))
  }

  test("Process.filter") {
    val even = filter((x: Int) => x % 2 == 0)
    even(LazyList(1, 2, 3, 4)).toList should be(List(2, 4))
  }

  test("Process.sum") {
    sum(LazyList(1.0, 2.0, 3.0, 4.0)).toList should be(
      List(1.0, 3.0, 6.0, 10.0)
    )
  }

}
