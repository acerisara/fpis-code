package fpis.code.chapter7

import fpis.code.chapter7.Par._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import java.util.concurrent.{ExecutorService, Executors}

@RunWith(classOf[JUnitRunner])
class ParTest extends AnyFunSuite {

  val es: ExecutorService = Executors.newFixedThreadPool(5)

  // TODO: Test fork()

  test("Par.sortPar") {
    val p1 = sortPar(unit(List(5, 4, 3, 2, 1)))
    p1(es).get() should be(List(1, 2, 3, 4, 5))

    val p2 = sortParM(unit(List(5, 4, 3, 2, 1)))
    p2(es).get() should be(List(1, 2, 3, 4, 5))
  }

  test("Par.parMap") {
    val p = parMap(List(1, 2, 3, 4, 5))(_.toString)
    p(es).get() should be(List("1", "2", "3", "4", "5"))
  }

  test("Par.parFilter") {
    val p = parFilter(List(1, 2, 3, 4, 5))(_ > 2)
    p(es).get() should be(List(3, 4, 5))
  }

  ignore("Par fork implementation deadlock") {
    val es = Executors.newFixedThreadPool(1)
    fork(lazyUnit(42 + 1))(es).get() should be(43)
  }

  ignore("Any fixed-size thread pool can be made to deadlock") {
    val n = 20 // arbitrary length
    val es = Executors.newFixedThreadPool(n)

    val a = lazyUnit(42 + 1)
    val wrapped = 0.until(n).foldLeft(a)((pa, _) => fork(pa))

    wrapped(es).get() should be(43)
  }
}
