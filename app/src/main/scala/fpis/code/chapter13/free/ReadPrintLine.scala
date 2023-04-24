package fpis.code.chapter13.free

import fpis.code.chapter11.Monad

import scala.io.StdIn.readLine

object ReadPrintLine {

  implicit val m: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = f(fa())
  }

  def ReadLine: Suspend[Function0, String] = Suspend[Function0, String] { () =>
    readLine()
  }

  def PrintLine(msg: String): Suspend[Function0, Unit] =
    Suspend[Function0, Unit] { () => println(msg) }

  def readPrintLine: Free[Function0, Unit] = for {
    _ <- PrintLine("Enter any string:")
    s <- ReadLine
    _ <- PrintLine(s"This is the string you entered: $s")
  } yield ()

  def main(args: Array[String]): Unit = {
    Free.runTrampoline(readPrintLine)
    Free.run(readPrintLine)
  }

}
