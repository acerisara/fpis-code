package fpis.code.chapter6

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Dispenser(locked: Boolean, candies: Int, coins: Int) {

  private def dispense(): Dispenser =
    if (!locked) Dispenser(locked = true, candies - 1, coins)
    else this

  private def addCoin(): Dispenser =
    if (locked && candies > 0) Dispenser(locked = false, candies, coins + 1)
    else this

  private def trigger(input: Input): Dispenser =
    input match {
      case Coin => addCoin()
      case Turn => dispense()
    }

}

object Dispenser {

  // TODO: Try using modify

  def next(
      dispenser: State[Dispenser, (Int, Int)],
      input: Input
  ): State[Dispenser, (Int, Int)] = for {
    _ <- dispenser
    dispenser <- State.get[Dispenser]
    nDispenser = dispenser.trigger(input)
    _ <- State.set(nDispenser)
  } yield (nDispenser.coins, nDispenser.candies)

  def simulateDispenser(inputs: List[Input]): State[Dispenser, (Int, Int)] =
    inputs.foldLeft(State.unit[Dispenser, (Int, Int)]((0, 0)))(next)

}
