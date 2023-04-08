package fpis.code.chapter12

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TraverseTest extends AnyFunSuite {

  private val lt = Traverse.listTraverse

  test("Traverse.zipWithIndex") {
    lt.zipWithIndex(List.empty) should be(List.empty)
    lt.zipWithIndex(List("a", "b", "c")) should be(
      List(("a", 0), ("b", 1), ("c", 2))
    )
  }

  test("Traverse.toList") {
    lt.toList(List.empty) should be(List.empty)
    lt.toList(List("a", "b", "c")) should be(
      List("a", "b", "c")
    )
  }

  test("Traverse.reverse") {
    lt.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  test("Traverse.foldLeft") {
    lt.foldLeft(List(1, 2, 3))(0)(_ + _) should be(6)
  }

}
