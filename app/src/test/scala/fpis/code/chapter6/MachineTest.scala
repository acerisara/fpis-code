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
      s.run(Machine(locked = true, candies = 5, coins = 10))._1

    result should be((14, 1))
  }

}
