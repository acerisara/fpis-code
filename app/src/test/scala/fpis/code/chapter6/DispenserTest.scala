package fpis.code.chapter6

import fpis.code.chapter6.Dispenser.simulateDispenser
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DispenserTest extends AnyFunSuite {

  test("6.11 simple run") {
    val s = simulateDispenser(
      List(
        Coin,
        Turn,
        Coin,
        Turn,
        Coin,
        Turn,
        Coin,
        Turn
      )
    )

    val result =
      s.run(Dispenser(locked = true, candies = 5, coins = 10))

    result should be((14, 1), Dispenser(locked = true, candies = 1, coins = 14))
  }

  test("turn on a locked dispenser") {
    val s = simulateDispenser(List(Turn))

    val result =
      s.run(Dispenser(locked = true, candies = 5, coins = 10))

    result should be((10, 5), Dispenser(locked = true, candies = 5, coins = 10))
  }

  test("insert a coin into an unlocked dispenser") {
    val s = simulateDispenser(List(Coin))

    val result =
      s.run(Dispenser(locked = false, candies = 5, coins = 10))

    result should be(
      (10, 5),
      Dispenser(locked = false, candies = 5, coins = 10)
    )
  }

  test("a dispenser that’s out of candy ignores all inputs") {
    val s = simulateDispenser(
      List(
        Coin,
        Turn,
        Coin,
        Turn
      )
    )

    val result =
      s.run(Dispenser(locked = true, candies = 0, coins = 10))

    result should be((10, 0), Dispenser(locked = true, candies = 0, coins = 10))
  }

  test("insert a coin into a locked dispenser") {
    val s = simulateDispenser(List(Coin))

    val result =
      s.run(Dispenser(locked = true, candies = 5, coins = 10))

    result should be(
      (11, 5),
      Dispenser(locked = false, candies = 5, coins = 11)
    )
  }

  test("turn the knob on an unlocked dispenser") {
    val s = simulateDispenser(List(Turn))

    val result =
      s.run(Dispenser(locked = false, candies = 5, coins = 10))

    result should be((10, 4), Dispenser(locked = true, candies = 4, coins = 10))
  }

}
