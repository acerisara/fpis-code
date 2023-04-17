package fpis.code.chapter13

object Forever {

  def main(args: Array[String]): Unit = {
    // This version overflows the stack
    val printLine = IO { println("Still going...") }
    IO.forever(printLine).run

    // This version has a tail recursive interpreter
    val printLine2 = Suspend(() => println("Still going..."))
    IO2.run(IO2.forever(printLine2))
  }

}
