package fpis.code.chapter13.free

import fpis.code.chapter11.Monad

import scala.io.StdIn.{readLine => readln}

object Echo {

  implicit val m: Monad[Function0] = new Monad[Function0] {
    override def unit[A](a: => A): () => A = () => a
    override def flatMap[A, B](fa: () => A)(f: A => () => B): () => B = f(fa())
  }

  def readLine: Suspend[Function0, String] = Suspend[Function0, String] { () =>
    readln()
  }

  def printLine(msg: String): Suspend[Function0, Unit] =
    Suspend[Function0, Unit] { () => println(msg) }

  def echo: Free[Function0, Unit] = for {
    _ <- printLine("Enter any string:")
    s <- readLine
    _ <- printLine(s"This is the string you entered: $s")
  } yield ()

  def main(args: Array[String]): Unit = {
    Free.runTrampoline(echo)
    Free.run(echo)
  }

}
