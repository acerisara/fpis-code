package fpis.code.chapter4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import scala.collection.immutable.List.empty

@RunWith(classOf[JUnitRunner])
class OptionTest extends AnyFunSuite {

  test("Option.map") {
    None.map(_.toString) should be(None)
    Some(1).map(_.toString) should be(Some("1"))
  }

  test("Option.flapMap") {
    None.flatMap((a: Int) => Some(a.toString)) should be(None)
    Some(1).flatMap(a => Some(a.toString)) should be(Some("1"))
  }

  test("Option.getOrElse") {
    None.getOrElse("default") should be("default")
    Some("1").getOrElse("default") should be("1")
  }

  test("Option.orElse") {
    None.orElse(Some("default")) should be(Some("default"))
    Some("1").orElse(Some("default")) should be(Some("1"))
  }

  test("Option.filter") {
    None.filter((a: Int) => a > 0) should be(None)
    Some(1).filter(_ > 0) should be(Some(1))
    Some(0).filter(_ > 0) should be(None)
  }

  test("Option.variance") {
    Option.variance(empty) should be(None)
    Option.variance(List(3, 5, 8, 1)) should be(Some(6.6875))
  }

  test("Option.sequence") {
    Option.sequence(List.empty) should be(Some(List.empty))
    Option.sequence(List(None)) should be(None)
    Option.sequence(List(Some(1), Some(2))) should be(Some(List(1, 2)))
    Option.sequence(List(Some(1), Some(2), None)) should be(None)
  }

}
