package fpis.code.chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  private def dispense(): Machine =
    if (!locked) Machine(locked = true, candies - 1, coins)
    else this

  private def addCoin(): Machine =
    if (locked && candies > 0) Machine(locked = false, candies, coins + 1)
    else this

  private def trigger(input: Input): Machine =
    input match {
      case Coin => addCoin()
      case Turn => dispense()
    }

}

object Machine {

  def next(
      machine: State[Machine, (Int, Int)],
      input: Input
  ): State[Machine, (Int, Int)] = for {
    _ <- machine
    dispenser <- State.get[Machine]
    nDispenser = dispenser.trigger(input)
    _ <- State.set(nDispenser)
  } yield (nDispenser.coins, nDispenser.candies)

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    inputs.foldLeft(State.unit[Machine, (Int, Int)]((0, 0)))(next)

}
