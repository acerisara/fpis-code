package fpis.code.chapter6

import fpis.code.chapter6.Machine.simulateMachine
import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MachineTest extends AnyFunSuite {

  test("6.11 simple run") {
    val s = simulateMachine(
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
      s.run(Machine(locked = true, candies = 5, coins = 10))

    result should be((14, 1), Machine(locked = true, candies = 1, coins = 14))
  }

  test("turn on a locked machine") {
    val s = simulateMachine(List(Turn))

    val result =
      s.run(Machine(locked = true, candies = 5, coins = 10))

    result should be((10, 5), Machine(locked = true, candies = 5, coins = 10))
  }

  test("insert a coin into an unlocked machine") {
    val s = simulateMachine(List(Coin))

    val result =
      s.run(Machine(locked = false, candies = 5, coins = 10))

    result should be((10, 5), Machine(locked = false, candies = 5, coins = 10))
  }

  test("a machine that’s out of candy ignores all inputs") {
    val s = simulateMachine(
      List(
        Coin,
        Turn,
        Coin,
        Turn
      )
    )

    val result =
      s.run(Machine(locked = true, candies = 0, coins = 10))

    result should be((10, 0), Machine(locked = true, candies = 0, coins = 10))
  }

  test("insert a coin into a locked machine") {
    val s = simulateMachine(List(Coin))

    val result =
      s.run(Machine(locked = true, candies = 5, coins = 10))

    result should be((11, 5), Machine(locked = false, candies = 5, coins = 11))
  }

  test("turn the knob on an unlocked machine") {
    val s = simulateMachine(List(Turn))

    val result =
      s.run(Machine(locked = false, candies = 5, coins = 10))

    result should be((10, 4), Machine(locked = true, candies = 4, coins = 10))
  }

}
