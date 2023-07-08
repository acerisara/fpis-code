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
    List.drop(0, List(1)) should be(List(1))
    List.drop(-1, List(1)) should be(List(1))
  }

  test("List.dropWhile") {
    val even = (x: Int) => x % 2 == 0

    List.dropWhile(Nil)(even) should be(Nil)
    List.dropWhile(List(2, 4, 6))(even) should be(Nil)
    List.dropWhile(List(1, 2, 3, 4, 5))(even) should be(List(1, 2, 3, 4, 5))
    List.dropWhile(List(2, 2, 3, 4, 5))(even) should be(List(3, 4, 5))
  }

  test("List.init") {
    List.init(Nil) should be(Nil)
    List.init(List(1)) should be(Nil)
    List.init(List(1, 2, 3)) should be(List(1, 2))
  }

  test("List.foldRight") {
    List.foldRight(Nil: List[Int], 0)(_ + _) should be(0)
    List.foldRight(List(1), 0)(_ + _) should be(1)
    List.foldRight(List(1, 2, 3), 0)(_ + _) should be(1 + 2 + 3)
  }

  ignore("List.foldRight overflows for big lists") {
    // implementation is not tail recursive, so not stack-safe
    val zeros = List(SList.fill(10000)(0): _*)
    List.foldRight(zeros, 0)(_ + _) should be(0)
  }

  test("List.foldRight via foldLeft and vice-versa") {
    val zeros = List(SList.fill(10000)(0): _*)
    List.foldRightFL(zeros, 0)(_ + _) should be(0)

    List.foldRight(List(1, 2, 3), 0)(_ - _) should be(2)
    List.foldRightFL(List(1, 2, 3), 0)(_ - _) should be(2)
    List.foldLeft(List(1, 2, 3), 0)(_ - _) should be(-6)
    List.foldLeftFR(List(1, 2, 3), 0)(_ - _) should be(-6)
  }

  test("Exercise 3.8") {
    val l = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    l should be(List(1, 2, 3))
  }

  test("List.length") {
    List.length(Nil) should be(0)
    List.length(List(1)) should be(1)
    List.length(List(1, 2, 3)) should be(3)
  }

  test("List.foldLeft") {
    List.foldLeft(Nil: List[Int], 0)(_ + _) should be(0)
    List.foldLeft(List(1), 0)(_ + _) should be(1)
    List.foldLeft(List(1, 2, 3), 0)(_ + _) should be(1 + 2 + 3)
  }

  test("List.foldLeft is stack-safe") {
    val zeros = List(SList.fill(10000)(0): _*)
    List.foldLeft(zeros, 0)(_ + _) should be(0)
  }

  test("List.reverse") {
    List.reverse(Nil) should be(Nil)
    List.reverse(List(1)) should be(List(1))
    List.reverse(List(1, 2, 3)) should be(List(3, 2, 1))
  }

  test("List.append") {
    List.append(Nil, Nil) should be(Nil)
    List.append(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.append(List(1, 2, 3), Nil) should be(List(1, 2, 3))
    List.append(List(1), List(2, 3)) should be(List(1, 2, 3))

    List.appendFR(Nil, Nil) should be(Nil)
    List.appendFR(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.appendFR(List(1, 2, 3), Nil) should be(List(1, 2, 3))
    List.appendFR(List(1), List(2, 3)) should be(List(1, 2, 3))

    List.appendFL(Nil, Nil) should be(Nil)
    List.appendFL(Nil, List(1, 2, 3)) should be(List(1, 2, 3))
    List.appendFL(List(1, 2, 3), Nil) should be(List(1, 2, 3))
    List.appendFL(List(1), List(2, 3)) should be(List(1, 2, 3))
  }

  test("List.flatten") {
    List.flatten(List(Nil)) should be(Nil)
    List.flatten(List(List(1), List(2), List(3))) should be(List(1, 2, 3))
    List.flatten(List(List(1, 2), List(3, 4), List(5, 6))) should be(
      List(1, 2, 3, 4, 5, 6)
    )
  }

  test("List.plus1") {
    List.plus1(Nil) should be(Nil)
    List.plus1(List(1)) should be(List(2))
    List.plus1(List(1, 2)) should be(List(2, 3))

    List.plus1FR(Nil) should be(Nil)
    List.plus1FR(List(1)) should be(List(2))
    List.plus1FR(List(1, 2)) should be(List(2, 3))
  }

  test("List.doubleToString") {
    List.doubleToString(Nil) should be(Nil)
    List.doubleToString(List(1.0)) should be(List("1.0"))
    List.doubleToString(List(1.0, 2.0, 3.0)) should be(
      List("1.0", "2.0", "3.0")
    )

    List.doubleToStringFR(Nil) should be(Nil)
    List.doubleToStringFR(List(1.0)) should be(List("1.0"))
    List.doubleToStringFR(List(1.0, 2.0, 3.0)) should be(
      List("1.0", "2.0", "3.0")
    )
  }

  test("List.map") {
    List.map(Nil: List[Double])(_.toString) should be(Nil)
    List.map(List(1.0))(_.toString) should be(List("1.0"))
    List.map(List(1.0, 2.0, 3.0))(_.toString) should be(
      List("1.0", "2.0", "3.0")
    )

    List.mapFR(Nil: List[Double])(_.toString) should be(Nil)
    List.mapFR(List(1.0))(_.toString) should be(List("1.0"))
    List.mapFR(List(1.0, 2.0, 3.0))(_.toString) should be(
      List("1.0", "2.0", "3.0")
    )
  }

  test("List.filter") {
    val even = (x: Int) => x % 2 == 0

    List.filter(Nil: List[Int])(even) should be(Nil)
    List.filter(List(1))(even) should be(Nil)
    List.filter(List(1, 2, 3, 4, 5, 6))(even) should be(List(2, 4, 6))

    List.filterFR(Nil: List[Int])(even) should be(Nil)
    List.filterFR(List(1))(even) should be(Nil)
    List.filterFR(List(1, 2, 3, 4, 5, 6))(even) should be(List(2, 4, 6))

    List.filterFM(Nil: List[Int])(even) should be(Nil)
    List.filterFM(List(1))(even) should be(Nil)
    List.filterFM(List(1, 2, 3, 4, 5, 6))(even) should be(List(2, 4, 6))
  }

  test("List.flatMap") {
    val duplicate = (x: Int) => List(x, x)

    List.flatMap(Nil: List[Int])(duplicate) should be(Nil)
    List.flatMap(List(1))(duplicate) should be(List(1, 1))
    List.flatMap(List(1, 2, 3))(duplicate) should be(List(1, 1, 2, 2, 3, 3))
  }

  test("List.zipInts") {
    List.zipInts(Nil, Nil) should be(Nil)
    List.zipInts(List(1), List(1)) should be(List(2))
    List.zipInts(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
    List.zipInts(List(1, 2), Nil) should be(List(1, 2))
    List.zipInts(Nil, List(1, 2)) should be(Nil)
    List.zipInts(List(1), List(1, 2, 3)) should be(List(2))
  }

  test("List.zipInts2") {
    List.zipInts2(Nil, Nil) should be(Nil)
    List.zipInts2(List(1), List(1)) should be(List(2))
    List.zipInts2(List(1, 2, 3), List(4, 5, 6)) should be(List(5, 7, 9))
    List.zipInts2(List(1, 2), Nil) should be(Nil)
    List.zipInts2(Nil, List(1, 2)) should be(Nil)
    List.zipInts2(List(1), List(1, 2, 3)) should be(List(2))
  }

  test("List.zipWith") {
    List.zipWith(List(1), Nil: List[Int])(_ + _) should be(Nil)
    List.zipWith(Nil: List[Int], List(1))(_ + _) should be(Nil)
    List.zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
    List.zipWith(List(1, 2, 3), List(4))(_ + _) should be(List(5))
    List.zipWith(List(1), List(4, 5, 6))(_ + _) should be(List(5))
  }

  test("List.hasSubsequence") {
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 2)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 3)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4), List(4)) should be(true)
    List.hasSubsequence(List(1, 2, 3, 4), List(1, 3)) should be(false)
    List.hasSubsequence(List(1, 2, 3, 4), List(2, 4)) should be(false)
    List.hasSubsequence(Nil, Nil) should be(false)
    List.hasSubsequence(List(1, 2, 3), Nil) should be(false)
    List.hasSubsequence(Nil, List(1, 2)) should be(false)
  }

}
