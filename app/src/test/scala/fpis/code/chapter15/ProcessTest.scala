package fpis.code.chapter15

import fpis.code.chapter15.Process._
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

import java.io.{File, PrintWriter}
import scala.io.Source

@RunWith(classOf[JUnitRunner])
class ProcessTest extends AnyFunSuite {

  test("Process.liftOne") {
    val double = (i: Int) => i * 2
    liftOne(double)(LazyList(1, 2, 3)).toList should be(List(2))
  }

  test("Process.filter") {
    val even = filter((x: Int) => x % 2 == 0)
    even(LazyList(1, 2, 3, 4)).toList should be(List(2, 4))
  }

  test("Process.sum") {
    sum(LazyList.empty).toList should be(
      List.empty
    )

    sum(LazyList(1.0, 2.0, 3.0, 4.0)).toList should be(
      List(1.0, 3.0, 6.0, 10.0)
    )

    sumL(LazyList.empty).toList should be(
      List.empty
    )

    sumL(LazyList(1.0, 2.0, 3.0, 4.0)).toList should be(
      List(1.0, 3.0, 6.0, 10.0)
    )
  }

  test("Process.take") {
    take(0)(LazyList(1, 2, 3)).toList should be(List.empty)
    take(2)(LazyList(1, 2, 3)).toList should be(List(1, 2))
  }

  test("Process.drop") {
    drop(0)(LazyList(1, 2, 3)).toList should be(List(1, 2, 3))
    drop(2)(LazyList(1, 2, 3)).toList should be(List(3))
  }

  test("Process.takeWhile") {
    takeWhile[Int](_ > 5)(LazyList(1, 2, 3, -1)).toList should be(List.empty)
    takeWhile[Int](_ > 0)(LazyList(1, 2, 3, -1)).toList should be(List(1, 2, 3))
  }

  test("Process.dropWhile") {
    dropWhile[Int](_ > 5)(LazyList(1, 2, 3, -1)).toList should be(
      List(1, 2, 3, -1)
    )

    dropWhile[Int](_ < 0)(LazyList(-1, -2, 1, 2, 3, -1)).toList should be(
      List(1, 2, 3)
    )
  }

  test("Process.count") {
    count[String](LazyList.empty).toList should be(
      List.empty
    )

    count[String](LazyList("a", "b", "c")).toList should be(
      List(1, 2, 3)
    )

    countL[String](LazyList.empty).toList should be(
      List.empty
    )

    countL[String](LazyList("a", "b", "c")).toList should be(
      List(1, 2, 3)
    )
  }

  test("Process.mean") {
    mean(LazyList.empty).toList should be(
      List.empty
    )

    mean(LazyList(1, 1, 1, 1)).toList should be(
      List(1, 1, 1, 1)
    )

    mean(LazyList(1, 2, 3, 4)).toList should be(
      List(1, 1.5, 2, 2.5)
    )
  }

  test("Process.pipe") {
    val p = filter[Int](_ % 2 == 0) |> lift[Int, Int](_ + 1)

    p(LazyList.empty).toList should be(List.empty)
    p(LazyList(1, 2, 3, 4)).toList should be(List(3, 5))
  }

  test("Process.zipWithIndex") {
    val p = filter[String](_ => true).zipWithIndex()

    p(LazyList.empty).toList should be(List.empty)
    p(LazyList("a", "b", "c")).toList should be(
      List((0, "a"), (1, "b"), (2, "c"))
    )
  }

  test("Process.exists") {
    val p = exists[Int](_ % 2 == 0)

    p(LazyList.empty).toList should be(List(false))
    p(LazyList(1, 3, 5, 7)).toList should be(List(false))
    p(LazyList(1, 3, 5, 6, 7)).toList should be(List(true))
  }

  test("Process.toCelsiusTransformer") {
    def newFile(prefix: String): File = {
      val f = File.createTempFile(prefix, null)
      f.deleteOnExit()
      f
    }

    def write[A](content: List[A], f: File): Unit = {
      val writer = new PrintWriter(f)
      try content.foreach(writer.println(_))
      finally writer.close()
    }

    def read(f: File): List[String] = {
      val source = Source.fromFile(f)
      try source.getLines().toList
      finally source.close()
    }

    val input = newFile("input")
    val output = newFile("output")
    write(List(32, 41, 50), input)

    toCelsiusTransformer(input, output).run
    read(output).map(_.toDouble) should be(List(0, 5, 10))
  }

}
