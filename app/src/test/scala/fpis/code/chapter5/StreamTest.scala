package fpis.code.chapter5

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StreamTest extends AnyFunSuite {

  test("Stream.toList") {
    Stream.empty.toList should be(List.empty)
    Stream(1).toList should be(List(1))
    Stream(1, 2, 3).toList should be(List(1, 2, 3))
  }

  test("Stream.headOption") {
    Stream.empty.headOption should be(None)
    Stream(1).headOption should be(Some(1))
  }

  test("Stream.take") {
    Stream.empty.take(0) should be(Stream.empty)
    Stream.empty.take(1) should be(Stream.empty)
    Stream(1).take(0).toList should be(List.empty)
    Stream(1).take(1).toList should be(List(1))
    Stream(1, 2, 3).take(2).toList should be(List(1, 2))
    Stream(1, 2, 3).take(3).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).take(5).toList should be(List(1, 2, 3))
  }

}
