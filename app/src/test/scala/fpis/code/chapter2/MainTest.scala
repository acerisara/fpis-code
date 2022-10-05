package fpis.code.chapter2

import fpis.code.chapter2.Main._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor2
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainTest extends AnyFunSuite {

  val first14FibonacciNumbers: TableFor2[Int, Int] =
    Table(
      ("input", "expected"),
      (0, 0),
      (1, 1),
      (2, 1),
      (3, 2),
      (4, 3),
      (5, 5),
      (6, 8),
      (7, 13),
      (8, 21),
      (9, 34),
      (10, 55),
      (11, 89),
      (12, 144),
      (13, 233)
    )

  val arrays: TableFor2[Array[Int], Boolean] =
    Table(
      ("input", "expected"),
      (Array(1, 2, 3, 4, 5), true),
      (Array(1, 1, 1, 1, 1), true),
      (Array(1, 2, 3, 5, 4), false),
      (Array(2, 1, 3, 4, 5), false),
      (Array(5, 4, 3, 2, 1), false),
      (Array(1, 2, 4, 3, 5), false)
    )

  test("Main.fib") {
    forAll(first14FibonacciNumbers) { (input, expected) =>
      fib(input) should equal(expected)
    }
  }

  test("Main.isSorted") {
    forAll(arrays) { (input, expected) =>
      isSorted(input)((x, y) => x <= y) should equal(expected)
    }
  }

  test("Main.curry") {
    val f = (a: Int, b: Int) => a + b
    f(2, 3) should equal(curry(f)(2)(3))
  }

  test("Main.uncurry") {
    val f = (a: Int, b: Int) => a + b
    uncurry(curry(f))(2, 3) should equal(f(2, 3))
  }

  test("Main.compose") {
    val g = (x: Int) => x + 1
    val f = (x: Int) => x * 2
    compose(f, g)(5) should be(f(g(5)))

    // Scala Function1 also provides `compose` and `andThen`
    (f compose g) (5) should be(f(g(5))) // `compose` is f(g(x))
    (f andThen g) (5) should be(g(f(5))) // `andThen` is g(f(x))
  }

}
