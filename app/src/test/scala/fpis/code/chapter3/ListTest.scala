package fpis.code.chapter3

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.{List => SList}

@RunWith(classOf[JUnitRunner])
class ListTest extends AnyFunSuite {

  ignore("List.sum overflows for big lists") {
    // implementation is not tail recursive, so not stack-safe
    val zeros = List(SList.fill(10000)(0): _*)
    List.sum(zeros) should be(0)
  }

  test("List.tail") {
    List.tail(Nil) should be(Nil)
    List.tail(List(1, 2, 3)) should be(List(2, 3))
    List.tail(List(1)) should be(Nil)
  }

  test("List.setHead") {
    List.setHead(1, Nil) should be(Nil)
    List.setHead(2, List(1)) should be(List(2))
    List.setHead(2, List(1, 2, 3)) should be(List(2, 2, 3))
  }

  test("List.drop") {
    List.drop(1, Nil) should be(Nil)
    List.drop(1, List(1)) should be(Nil)
    List.drop(2, List(1, 2, 3)) should be(List(3))
  }

  test("List.dropWhile") {
    val even = (x: Int) => x % 2 == 0

    List.dropWhile(Nil)(even) should be(Nil)
    List.dropWhile(List(2, 4, 6))(even) should be(Nil)
    List.dropWhile(List(1, 2, 3, 4, 5))(even) should be(List(1, 2, 3, 4, 5))
    List.dropWhile(List(2, 2, 3, 4, 5))(even) should be(List(3, 4, 5))
  }

  test("List.append") {
    List.append(Nil, Nil) should be(Nil)
    List.append(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.append(List(1, 2, 3), Nil) should be(List(1, 2, 3))
    List.append(List(1), List(2, 3)) should be(List(1, 2, 3))
  }

  test("List.init") {
    List.init(Nil) should be(Nil)
    List.init(List(1)) should be(Nil)
    List.init(List(1, 2, 3)) should be(List(1, 2))
  }

}
