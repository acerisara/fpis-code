package fpis.code.chapter14

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class STTest extends AnyFunSuite {

  test("simple program to swap ints") {
    def swap(t: (Int, Int)): RunnableST[(Int, Int)] =
      new RunnableST[(Int, Int)] {
        def apply[S]: ST[S, (Int, Int)] = for {
          r1 <- STRef(t._1)
          r2 <- STRef(t._2)
          x <- r1.read
          y <- r2.read
          _ <- r1.write(y)
          _ <- r2.write(x)
          a <- r1.read
          b <- r2.read
        } yield (a, b)
      }

    ST.runST(swap((1, 2))) should be((2, 1))
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

  test("STArray.swap") {
    val p = new RunnableST[List[String]] {
      def apply[S]: ST[S, List[String]] = for {
        array <- STArray.fromList(List("a", "b", "c"))
        _ <- array.swap(0, 1)
        asList <- array.freeze
      } yield asList
    }

    ST.runST(p) should be(List("b", "a", "c"))
  }

  test("STMap.get") {
    val p = new RunnableST[Option[String]] {
      def apply[S]: ST[S, Option[String]] = for {
        map <- STMap(1 -> "a", 2 -> "b", 3 -> "c")
        v <- map.get(1)
      } yield v
    }

    ST.runST(p) should be(Some("a"))
  }

  test("STMap.put") {
    val p = new RunnableST[Option[String]] {
      def apply[S]: ST[S, Option[String]] = for {
        map <- STMap(1 -> "a", 2 -> "b", 3 -> "c")
        _ <- map.put(2, "B")
        v <- map.get(2)
      } yield v
    }

    ST.runST(p) should be(Some("B"))
  }

}
