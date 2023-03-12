package fpis.code.chapter10

import fpis.code.chapter11.Monad
import fpis.code.chapter11.Monad.optionMonad
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MonadTest extends AnyFunSuite {

  val om: Monad[Option] = optionMonad

  test("Monad.sequence") {
    om.sequence(List(Some(1), Some(2), Some(3))) should be(Some(List(1, 2, 3)))
    om.sequence(List(Some(1), None, Some(3))) should be(None)
  }

  test("Monad.traverse") {
    val f = (i: Int) => if (i < 5) Some(i) else None

    om.traverse(List(1, 2, 3))(f) should be(Some(List(1, 2, 3)))
    om.traverse(List(1, 6, 2))(f) should be(None)
  }

  test("Monad.replicatedM") {
    om.replicateM(3, Some(1)) should be(Some(List(1, 1, 1)))
    om.replicateM(3, None) should be(None)
  }

  test("Monad.product") {
    om.product(Some(1), Some("a")) should be(Some((1, "a")))
    om.product(Some(1), None) should be(None)
  }

  test("Monad.filterM") {
    val f = (i: Int) => if (i < 5) Some(true) else Some(false)
    val n = (i: Int) => if (i < 5) Some(true) else None

    om.filterM(List(1, 2, 3))(f) should be(Some(List(1, 2, 3)))
    om.filterM(List(1, 6, 3))(f) should be(Some(List(1, 3)))
    om.filterM(List(1, 6, 3))(n) should be(None)
  }

  test("Monad.compose") {
    val f = (i: Int) => if (i > 0) Some(i.toString) else Some("")
    val g = (s: String) => if (s.nonEmpty) Some(true) else Some(false)

    val h = om.compose(f, g)

    h(1) should be(Some(true))
    h(0) should be(Some(false))
  }

}
