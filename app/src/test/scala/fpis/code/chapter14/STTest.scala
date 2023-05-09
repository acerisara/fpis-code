package fpis.code.chapter14

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class STTest extends AnyFunSuite {

  test("simple program to swap ints") {
    val p = new RunnableST[(Int, Int)] {
      def apply[S]: ST[S, (Int, Int)] = for {
        r1 <- STRef(1)
        r2 <- STRef(2)
        x <- r1.read
        y <- r2.read
        _ <- r1.write(y)
        _ <- r2.write(x)
        a <- r1.read
        b <- r2.read
      } yield (a, b)
    }

    ST.runST(p) should be((2, 1))
  }

  test("STArray.fill") {
    val p = new RunnableST[List[String]] {
      def apply[S]: ST[S, List[String]] = for {
        array <- STArray.fromList(List("a", "a", "a"))
        _ <- array.fill(Map(1 -> "b", 2 -> "c"))
        asList <- array.freeze
      } yield asList
    }

    ST.runST(p) should be(List("a", "b", "c"))
  }

}
