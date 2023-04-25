package fpis.code.chapter13.async

import fpis.code.chapter7.Par

import java.util.concurrent.{ExecutorService, Executors}
import scala.io.StdIn.{readLine => readln}

object Echo {

  val es: ExecutorService = Executors.newFixedThreadPool(5)

  def readLine: Suspend[String] = Suspend(Par.lazyUnit(readln()))

  def printLine(msg: String): Suspend[Unit] = Suspend(
    Par.lazyUnit(println(msg))
  )

  def echo: Async[Unit] = for {
    _ <- printLine("Enter any string:")
    s <- readLine
    _ <- printLine(s"This is the string you entered: $s")
  } yield ()

  def main(args: Array[String]): Unit = {
    Async.run(echo)(es).get()
    es.shutdown()
  }

}
