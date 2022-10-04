package fpis.code.chapter2

import fpis.code.chapter2.Main.fib
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers.{convertToAnyShouldWrapper, equal}
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor2
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MainTest extends AnyFunSuite {

  val first14FibonacciNumbers: TableFor2[Int, Int] =
    Table(
      ("n", "f"),
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

  test("Main.fib should compute the Fibonacci sequence") {
    forAll(first14FibonacciNumbers) { (n, f) =>
      fib(n) should equal(f)
    }
  }

}
