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

  def next(
      dispenser: State[Dispenser, Unit],
      input: Input
  ): State[Dispenser, Unit] = for {
    _ <- dispenser
    _ <- State.modify[Dispenser](_.trigger(input))
  } yield ()

  def simulateDispenser(inputs: List[Input]): State[Dispenser, Unit] =
    inputs.foldLeft(State.unit[Dispenser, Unit](()))(next)

}
