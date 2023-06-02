package fpis.code.chapter14

import fpis.code.chapter14.QuickSort.quicksort
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class QuickSortTest extends AnyFunSuite {

  test("QuickSort") {
    quicksort(List()) should be(List())
    quicksort(List(1)) should be(List(1))
    quicksort(List(1, 2)) should be(List(1, 2))
    quicksort(List(2, 1)) should be(List(1, 2))
    quicksort(List(4, 3, 2, 1)) should be(List(1, 2, 3, 4))
    quicksort(List(1, 1, 2, 1)) should be(List(1, 1, 1, 2))
  }

}
