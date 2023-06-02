package fpis.code.chapter4

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class EitherTest extends AnyFunSuite {

  val error: Either[String, Int] = Left("boom!")

  test("Either.map") {
    error.map(_.toString) should be(error)
    Right(1).map(_.toString) should be(Right("1"))
  }

  test("Either.flatMap") {
    error.flatMap(a => Right(a.toString)) should be(error)
    Right(1).flatMap(a => Right((a * 2).toString)) should be(Right("2"))
  }

  test("Either.orElse") {
    error.orElse(Right(1)) should be(Right(1))
    Right(1).orElse(error) should be(Right(1))
  }

  test("Either.map2") {
    val one = Right(1)
    val add = (x: Int, y: Int) => x + y

    error.map2(one)(add) should be(error)
    one.map2(error)(add) should be(error)
    one.map2(one)(add) should be(Right(2))
  }

  test("Either.sequence") {
    val one = Right(1)

    Either.sequence(List.empty) should be(Right(List.empty))
    Either.sequence(List(error)) should be(error)
    Either.sequence(List(one, one)) should be(Right(List(1, 1)))
    Either.sequence(List(one, error, one)) should be(error)

    Either.sequenceT(List.empty) should be(Right(List.empty))
    Either.sequenceT(List(error)) should be(error)
    Either.sequenceT(List(one, one)) should be(Right(List(1, 1)))
    Either.sequenceT(List(one, error, one)) should be(error)
  }

  test("Either.traverse") {
    val toString = (x: Int) =>
      if (x == 1) Right(x.toString)
      else Left("boom!")

    Either.traverse(List.empty)(toString) should be(Right(List.empty))
    Either.traverse(List(1))(toString) should be(Right(List("1")))
    Either.traverse(List(1, 2))(toString) should be(Left("boom!"))
  }

}
