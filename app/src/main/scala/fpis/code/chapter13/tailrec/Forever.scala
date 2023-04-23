package fpis.code.chapter13.tailrec

object Forever {

  def main(args: Array[String]): Unit = {
    val printLine = Suspend { () => println("Still going...") }
    TailRec.run(TailRec.forever(printLine))
  }

}
