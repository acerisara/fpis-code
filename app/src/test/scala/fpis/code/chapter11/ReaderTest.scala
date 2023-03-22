package fpis.code.chapter11

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ReaderTest extends AnyFunSuite {

  val rm: Monad[
    ({
      type f[x] = Reader[Int, x]
    })#f
  ] = Reader.readerMonad[Int]

  test("Reader monad sequence") {
    // sequence runs n function passing the same
    // input value and returns the results of each invocation
    val s = rm.sequence(
      List(
        Reader((r: Int) => s"1x ${r.toString}"),
        Reader((r: Int) => s"2x ${r.toString}"),
        Reader((r: Int) => s"3x ${r.toString}")
      )
    )

    s.run(10) should be(List("1x 10", "2x 10", "3x 10"))
  }

  test("Reader monad replicateM") {
    // replicateM runs the same function n times
    // and returns the results of each invocation
    val s = rm.replicateM(3, Reader(_.toString))

    s.run(10) should be(List("10", "10", "10"))
  }

  test("Reader monad join") {
    val s =
      rm.join(Reader((r1: Int) => Reader((r2: Int) => (r1 + r2).toString)))

    s.run(10) should be("20")
  }

}
