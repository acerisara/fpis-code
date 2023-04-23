package fpis.code.chapter13.async

import fpis.code.chapter7.Par

import java.util.concurrent.{ExecutorService, Executors}
import scala.io.StdIn.readLine

object ReadPrintLine {

  val es: ExecutorService = Executors.newFixedThreadPool(5)

  def ReadLine: Suspend[String] = Suspend(Par.lazyUnit(readLine()))

  def PrintLine(msg: String): Suspend[Unit] = Suspend(
    Par.lazyUnit(println(msg))
  )

  def readPrintLine: Async[Unit] = for {
    _ <- PrintLine("Enter any string:")
    s <- ReadLine
    _ <- PrintLine(s"This is the string you entered: $s")
  } yield ()

  def main(args: Array[String]): Unit = {
    Async.run(readPrintLine)(es).get()
    es.shutdown()
  }

}
