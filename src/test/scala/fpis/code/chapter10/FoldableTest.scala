package fpis.code.chapter10

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FoldableTest extends AnyFunSuite {

  val stringMonoid: Monoid[String] = Monoid.stringMonoid
  val intAdditionMonoid: Monoid[Int] = Monoid.intAdditionMonoid
  val listFoldable: Foldable[List] = Foldable.foldableList

  test("Foldable.foldRight") {
    listFoldable.foldRight(List.empty[Integer])(0)(_ + _) should be(0)
    listFoldable.foldRight(List(1, 2, 3))(0)(_ + _) should be(6)
    listFoldable.foldRight(List(1, 2, 3))(0)(_ - _) should be(2)
  }

  test("Foldable.foldLeft") {
    listFoldable.foldLeft(List.empty[Integer])(0)(_ + _) should be(0)
    listFoldable.foldLeft(List(1, 2, 3))(0)(_ + _) should be(6)
    listFoldable.foldLeft(List(1, 2, 3))(0)(_ - _) should be(-6)
  }

  test("Foldable.toList") {
    listFoldable.toList(List.empty) should be(List.empty)
    listFoldable.toList(List(1, 2, 3)) should be(List(1, 2, 3))
  }

  test("Foldable.concatenate") {
    listFoldable.concatenate(List.empty)(intAdditionMonoid) should be(0)
    listFoldable.concatenate(List(1, 2, 3))(intAdditionMonoid) should be(6)
  }

  test("Foldable.foldMap") {
    listFoldable.foldMap(List.empty[Integer])(_.toString)(
      stringMonoid
    ) should be("")

    listFoldable.foldMap(List(1, 2, 3))(_.toString)(
      stringMonoid
    ) should be("123")
  }

}
