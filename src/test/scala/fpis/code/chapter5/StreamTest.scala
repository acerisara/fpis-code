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
    Stream(1, 2, 3).headOption should be(Some(1))

    Stream.empty.headOptionF should be(None)
    Stream(1).headOptionF should be(Some(1))
    Stream(1, 2, 3).headOptionF should be(Some(1))
  }

  test("Stream.take") {
    Stream.empty.take(0).toList should be(List.empty)
    Stream.empty.take(1).toList should be(List.empty)
    Stream(1).take(0).toList should be(List.empty)
    Stream(1).take(1).toList should be(List(1))
    Stream(1, 2, 3).take(2).toList should be(List(1, 2))
    Stream(1, 2, 3).take(3).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).take(5).toList should be(List(1, 2, 3))

    Stream.empty.takeU(0).toList should be(List.empty)
    Stream.empty.takeU(1).toList should be(List.empty)
    Stream(1).takeU(0).toList should be(List.empty)
    Stream(1).takeU(1).toList should be(List(1))
    Stream(1, 2, 3).takeU(2).toList should be(List(1, 2))
    Stream(1, 2, 3).takeU(3).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).takeU(5).toList should be(List(1, 2, 3))
  }

  test("Stream.takeWhile") {
    Stream.empty[Boolean].takeWhile(_ == true).toList should be(List.empty)
    Stream(1).takeWhile(_ > 0).toList should be(List(1))
    Stream(1).takeWhile(_ > 1).toList should be(List.empty)
    Stream(1, 2, 3, 4, 5).takeWhile(_ < 4).toList should be(List(1, 2, 3))

    Stream.empty[Boolean].takeWhileF(_ == true).toList should be(List.empty)
    Stream(1).takeWhileF(_ > 0).toList should be(List(1))
    Stream(1).takeWhileF(_ > 1).toList should be(List.empty)
    Stream(1, 2, 3, 4, 5).takeWhileF(_ < 4).toList should be(List(1, 2, 3))

    Stream.empty[Boolean].takeWhileU(_ == true).toList should be(List.empty)
    Stream(1).takeWhileU(_ > 0).toList should be(List(1))
    Stream(1).takeWhileU(_ > 1).toList should be(List.empty)
    Stream(1, 2, 3, 4, 5).takeWhileU(_ < 4).toList should be(List(1, 2, 3))
  }

  test("Stream.forAll") {
    Stream.empty[Boolean].forAll(_ == true) should be(true)
    Stream(1).forAll(_ > 0) should be(true)
    Stream(1, 2, 3).forAll(_ > 0) should be(true)
    Stream(1, 2, 3).forAll(_ > 2) should be(false)
  }

  test("Stream.map") {
    Stream.empty[Int].map(_.toString).toList should be(List.empty)
    Stream(1).map(_.toString).toList should be(List("1"))
    Stream(1, 2, 3).map(_.toString).toList should be(List("1", "2", "3"))

    Stream.empty[Int].mapU(_.toString).toList should be(List.empty)
    Stream(1).mapU(_.toString).toList should be(List("1"))
    Stream(1, 2, 3).mapU(_.toString).toList should be(List("1", "2", "3"))
  }

  test("Stream.filter") {
    Stream.empty[Int].filter(_ > 0).toList should be(List.empty)
    Stream(1).filter(_ > 0).toList should be(List(1))
    Stream(1, 2, 3).filter(_ > 0).toList should be(List(1, 2, 3))
    Stream(1, 2, 3, 4, 5).filter(_ % 2 == 0).toList should be(List(2, 4))
  }

  test("Stream.append") {
    Stream.empty.append(Stream.empty).toList should be(List.empty)
    Stream.empty.append(Stream(1, 2, 3)).toList should be(List(1, 2, 3))
    Stream(1, 2, 3).append(Stream.empty).toList should be(List(1, 2, 3))
    Stream(1, 2).append(Stream(3, 4)).toList should be(List(1, 2, 3, 4))
  }

  test("Stream.flatMap") {
    Stream.empty[Int].flatMap(_ => Stream.empty).toList should be(List.empty)
    Stream(1, 2, 3).flatMap(_ => Stream.empty).toList should be(List.empty)
    Stream.empty[Int].flatMap(_ => Stream(1)).toList should be(List.empty)
    Stream(1, 2).flatMap(i => Stream(i, i)).toList should be(List(1, 1, 2, 2))
  }

  test("Stream.from") {
    Stream.from(0).take(0).toList should be(List.empty)
    Stream.from(0).take(3).toList should be(List(0, 1, 2))
    Stream.from(5).take(3).toList should be(List(5, 6, 7))
    Stream.fromU(0).take(0).toList should be(List.empty)
    Stream.fromU(0).take(3).toList should be(List(0, 1, 2))
    Stream.fromU(5).take(3).toList should be(List(5, 6, 7))
  }

  test("Stream.fibs") {
    Stream.fibs().take(0).toList should be(List.empty)
    Stream.fibs().take(1).toList should be(List(0))
    Stream.fibs().take(2).toList should be(List(0, 1))
    Stream.fibs().take(3).toList should be(List(0, 1, 1))
    Stream.fibs().take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))

    Stream.fibsU().take(0).toList should be(List.empty)
    Stream.fibsU().take(1).toList should be(List(0))
    Stream.fibsU().take(2).toList should be(List(0, 1))
    Stream.fibsU().take(3).toList should be(List(0, 1, 1))
    Stream.fibsU().take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
  }

  test("Stream.ones") {
    Stream.ones.take(3).toList should be(List(1, 1, 1))
    Stream.onesU.take(3).toList should be(List(1, 1, 1))
  }

  test("Stream.constant") {
    Stream.constant("S").take(3).toList should be(List("S", "S", "S"))
    Stream.constant(()).take(3).toList should be(List((), (), ()))
    Stream.constantU("S").take(3).toList should be(List("S", "S", "S"))
    Stream.constantU(()).take(3).toList should be(List((), (), ()))
  }

  test("Stream.zipWith") {
    Stream.empty[Int].zipWith(Stream.empty[Int])(_ + _).toList should be(
      List.empty
    )

    Stream(1, 2, 3).zipWith(Stream.empty[Int])(_ + _).toList should be(
      List.empty
    )

    Stream.empty[Int].zipWith(Stream(1, 2, 3))(_ + _).toList should be(
      List.empty
    )

    Stream(1, 1, 1).zipWith(Stream(2, 2, 2))(_ + _).toList should be(
      List(3, 3, 3)
    )

    Stream(1, 1).zipWith(Stream(2, 2, 2))(_ + _).toList should be(
      List(3, 3)
    )

    Stream(1, 1, 1).zipWithU(Stream(2, 2, 2))(_ + _).toList should be(
      List(3, 3, 3)
    )

    Stream(1, 1).zipWithU(Stream(2, 2, 2))(_ + _).toList should be(
      List(3, 3)
    )
  }

  test("Stream.zipAll") {
    Stream.empty.zipAll(Stream.empty).toList should be(List.empty)
    Stream.empty.zipAllU(Stream.empty).toList should be(List.empty)

    Stream(1, 2, 3).zipAll(Stream.empty).toList should be(
      List((Some(1), None), (Some(2), None), (Some(3), None))
    )

    Stream(1, 2, 3).zipAllU(Stream.empty).toList should be(
      List((Some(1), None), (Some(2), None), (Some(3), None))
    )

    Stream.empty.zipAll(Stream(1, 2, 3)).toList should be(
      List((None, Some(1)), (None, Some(2)), (None, Some(3)))
    )

    Stream.empty.zipAllU(Stream(1, 2, 3)).toList should be(
      List((None, Some(1)), (None, Some(2)), (None, Some(3)))
    )

    Stream(1, 2, 3).zipAll(Stream(1, 2)).toList should be(
      List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
    )

    Stream(1, 2, 3).zipAllU(Stream(1, 2)).toList should be(
      List((Some(1), Some(1)), (Some(2), Some(2)), (Some(3), None))
    )
  }

  test("Stream.startsWith") {
    Stream.empty.startsWith(Stream.empty) should be(true)
    Stream.empty.startsWith(Stream(1)) should be(false)
    Stream(1).startsWith(Stream(1)) should be(true)
    Stream(1, 2, 3).startsWith(Stream(1, 2)) should be(true)
    Stream(1).startsWith(Stream(1, 2)) should be(false)
    Stream(1).startsWith(Stream.empty) should be(true)
  }

  test("Stream.tails") {
    Stream.empty[Int].tails.map(_.toList).toList should be(List(List.empty))
    Stream(1).tails.map(_.toList).toList should be(List(List(1), List.empty))

    Stream(1, 2, 3).tails.map(_.toList).toList should be(
      List(
        List(1, 2, 3),
        List(2, 3),
        List(3),
        List.empty
      )
    )
  }

  test("Stream.scanRight") {
    Stream.empty[Int].scanRight(0)(_ + _).toList should be(List(0))
    Stream(1).scanRight(0)(_ + _).toList should be(List(1, 0))
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should be(List(6, 5, 3, 0))
  }

}
