package fpis.code.chapter12

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TraverseTest extends AnyFunSuite {

  test("Traverse.zipWithIndex") {
    val lt = Traverse.listTraverse

    lt.zipWithIndex(List.empty) should be(List.empty)
    lt.zipWithIndex(List("a", "b", "c")) should be(
      List(("a", 0), ("b", 1), ("c", 2))
    )
  }

  test("Traverse.toList") {
    val lt = Traverse.listTraverse

    lt.toList(List.empty) should be(List.empty)
    lt.toList(List("a", "b", "c")) should be(
      List("a", "b", "c")
    )
  }

}
