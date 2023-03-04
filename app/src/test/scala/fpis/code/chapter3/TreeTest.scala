package fpis.code.chapter3

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import scala.{List => SList}

@RunWith(classOf[JUnitRunner])
class TreeTest extends AnyFunSuite {

  test("Tree.size") {
    Tree.size(Leaf(1)) should be(1)
    Tree.size(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(5)

    Tree.sizeF(Leaf(1)) should be(1)
    Tree.sizeF(Branch(Leaf(1), Leaf(2))) should be(3)
    Tree.sizeF(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(5)
  }

  test("Tree.maximum") {
    Tree.maximum(Leaf(1)) should be(1)
    Tree.maximum(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))) should be(10)

    Tree.maximumF(Leaf(1)) should be(1)
    Tree.maximumF(Branch(Leaf(1), Leaf(2))) should be(2)
    Tree.maximumF(Branch(Branch(Leaf(1), Leaf(2)), Leaf(10))) should be(10)
  }

  test("Tree.depth") {
    Tree.depth(Leaf(1)) should be(0)
    Tree.depth(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(2)

    Tree.depthF(Leaf(1)) should be(0)
    Tree.depthF(Branch(Leaf(1), Leaf(2))) should be(1)
    Tree.depthF(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(2)
  }

  test("Tree.map") {
    Tree.map(Leaf(1))(_.toString) should be(Leaf("1"))
    Tree.map(Branch(Leaf(1), Leaf(2)))(_.toString) should be(
      Branch(Leaf("1"), Leaf("2"))
    )

    Tree.mapF(Leaf(1))(_.toString) should be(Leaf("1"))
    Tree.mapF(Branch(Leaf(1), Leaf(2)))(_.toString) should be(
      Branch(Leaf("1"), Leaf("2"))
    )
  }

  test("Tree.toList") {
    Tree.toList(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) should be(
      SList(1, 2, 3)
    )
  }

}
