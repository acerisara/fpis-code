package fpis.code.chapter13.io

object Forever {

  def main(args: Array[String]): Unit = {
    val printLine = IO { println("Still going...") }
    IO.forever(printLine).run
  }

}
