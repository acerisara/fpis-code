package fpis.code.chapter13.free

object Forever {

  def main(args: Array[String]): Unit = {
    val printLine = Suspend(PrintLine("Still going..."))
    Console.runConsoleFunction0(Free.forever(printLine))()
  }

}
